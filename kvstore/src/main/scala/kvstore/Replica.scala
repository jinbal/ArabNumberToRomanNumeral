package kvstore

import akka.actor.SupervisorStrategy.{Restart, Resume}
import akka.actor.{Actor, ActorRef, Cancellable, OneForOneStrategy, Props, ReceiveTimeout, Terminated}
import kvstore.Arbiter._
import kvstore.Persistence.{Persist, Persisted, PersistenceException}
import kvstore.Replicator.{Replicate, Replicated, Snapshot, SnapshotAck}

import scala.concurrent.duration._

object Replica {

  sealed trait Operation {
    def key: String

    def id: Long
  }

  case class Insert(key: String, value: String, id: Long) extends Operation

  case class Remove(key: String, id: Long) extends Operation

  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply

  case class OperationAck(id: Long) extends OperationReply

  case class OperationFailed(id: Long) extends OperationReply

  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {

  import Replica._
  import context.dispatcher
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 100, withinTimeRange = 1 second) {
      case _: PersistenceException => Resume
    }
  val persistence = context.actorOf(persistenceProps)
  context.watch(persistence)
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  var _seqCounter = 0L


  def nextSeq() = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  arbiter ! Join

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Get(key, id) =>
      sender() ! GetResult(key, kv.get(key), id)
    case Insert(key, value, id) =>
      kv += (key -> value)
      val persist = Persist(key, Some(value), id)
      context.setReceiveTimeout(1000 milliseconds)
      val cancellable: Cancellable = context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, persistence, persist)
      context.become(persistingLeader(sender(), persist,cancellable))
    case Remove(key, id) =>
      kv -= (key)
      val persist = Persist(key, None, id)
      context.setReceiveTimeout(1000 milliseconds)
      val cancellable: Cancellable = context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, persistence, persist)
      context.become(persistingLeader(sender(), persist, cancellable))
    case Replicas(replicasIncPrimary) =>
      val replicas = replicasIncPrimary - self
      val (remaining, removed) = secondaries.partition(r => replicas.contains(r._1))
      val newReps = replicas.filter(r => !remaining.contains(r)).map { rep =>
        (rep, context.actorOf(Replicator.props(rep)))
      }.toMap
      val fullyUpdated = remaining ++ newReps
      secondaries = fullyUpdated
      replicators = fullyUpdated.map(_._2).toSet
      val removedReplicas = removed.map(_._2).toSet
      stopRemovedReplicators(removedReplicas)
    case _ =>
  }

  private def stopRemovedReplicators(removed: Set[ActorRef]) = {
    removed.foreach(context.stop)
  }

  def persistingLeader(replyTo: ActorRef, persist: Persist, cancellable: Cancellable): Receive = {
    case Persisted(_, id) =>
      cancellable.cancel()
      synchReplicas(replyTo, persist)
    case Get(key, id) =>
      sender() ! GetResult(key, kv.get(key), id)
    case ReceiveTimeout =>
      cancellable.cancel()
      replyTo ! OperationFailed(persist.id)
      context.become(leader)
  }

  def synchReplicas(replyTo: ActorRef, persist: Persist): Unit = {
    if(replicators.isEmpty) {
      replyTo ! OperationAck(persist.id)
      context.become(leader)
    } else {
      val replicationMessages: Map[Long, Cancellable] = replicators.map { r =>
        val id = nextSeq()
        val message = Replicate(persist.key, persist.valueOption, id)
        val cancellable: Cancellable = context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, r, message)
        (id, cancellable)
      }.toMap

      context.become(waitForReplicaSynch(replyTo, replicationMessages, persist.id))
    }
  }

  def waitForReplicaSynch(replyTo: ActorRef, messages: Map[Long, Cancellable], ackId: Long): Receive = {
    case Replicated(_, id) =>
      messages.get(id).map(_.cancel())
      val pending = messages - id
      if (pending.isEmpty) {
        replyTo ! OperationAck(ackId)
        context.become(leader)
      } else {
        context.become(waitForReplicaSynch(replyTo, pending, ackId))
      }
    case OperationFailed(id) =>
      messages.get(id).map(_.cancel())
      replyTo ! OperationFailed(ackId)
      context.become(leader)
    case ReceiveTimeout =>
      replyTo ! OperationFailed(ackId)
    case _ =>
//      println(s"unexpected")
  }

  def persistingReplica(replyTo: ActorRef, persist: Persist, cancellable: Cancellable): Receive = {
    case Persisted(key, seq) =>
      cancellable.cancel()
      replyTo ! SnapshotAck(key, seq)
      context.become(replica)
    case Get(key, id) =>
      sender() ! GetResult(key, kv.get(key), id)
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) =>
      sender() ! GetResult(key, kv.get(key), id)
    case Snapshot(key, valueOption, seq) =>
      if (seq <= _seqCounter) {
        if (seq == _seqCounter) {
          valueOption match {
            case Some(v) => kv += (key -> v)
            case None => kv -= (key)
          }
          _seqCounter = seq + 1
        }
        val persist = Persist(key, valueOption, seq)
        val cancellable: Cancellable = context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, persistence, persist)
        context.become(persistingReplica(sender(), persist, cancellable))
      }
    case _ =>
  }
}

