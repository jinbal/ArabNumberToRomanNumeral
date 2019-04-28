package convert

object RomanNumeralConverters {

  case class ConversionResult(remaining: Int, output: String = "")

  sealed abstract class Numeral(symbol: String, numericValue: Int) {

    def append(conversionResult: ConversionResult): ConversionResult = {
      val remainder = conversionResult.remaining % numericValue
      val divisible = conversionResult.remaining - remainder
      val symbolRepeat = divisible / numericValue
      val newOutput = List.fill(symbolRepeat)(symbol).mkString
      conversionResult.copy(remaining = remainder, output = conversionResult.output + newOutput)
    }

  }


  case object One extends Numeral(symbol = "I", 1)

  case object Four extends Numeral(symbol = "IV", 4)

  case object Five extends Numeral(symbol = "V", 5)

  case object Nine extends Numeral(symbol = "IX", 9)

  case object Ten extends Numeral(symbol = "X", 10)

  case object Forty extends Numeral(symbol = "XL", 40)

  case object Fifty extends Numeral(symbol = "L", 50)

  case object Ninety extends Numeral(symbol = "XC", 90)

  case object Hundred extends Numeral(symbol = "C", 100)

  case object FourHundred extends Numeral(symbol = "CD", 400)

  case object FiveHundred extends Numeral(symbol = "D", 500)

  case object NineHundred extends Numeral(symbol = "CM", 900)

  case object Thousand extends Numeral(symbol = "M", 1000)

  val AllNumerals = List(
                          Thousand,
                          NineHundred,
                          FiveHundred,
                          FourHundred,
                          Hundred,
                          Ninety,
                          Fifty,
                          Forty,
                          Ten,
                          Nine,
                          Five,
                          Four,
                          One
                        )


  def convert(numericValue: Int): ConversionResult = {
    val intialResult = ConversionResult(numericValue)

    AllNumerals.foldLeft(intialResult) { case (currentResult, numeral) =>
      numeral.append(currentResult)
    }
  }
}
