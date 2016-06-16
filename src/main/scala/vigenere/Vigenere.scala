/* Algebraic Vigenere cypher */

object Vigenere {
  val Alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ. :/"
  val AlphaLen = Alpha.length()
  val Key = "FOUDIL"  // 0x464F5544494C
  val Text  = "NLUPF/TLWRWWCVJWX/DNTJQAMEV/KZRNZR:OK:KC"
  val Clear = "I AM SO COOL HTTPS://GITHUB.COM/FOUDFOU/"
  //          "FOUDILFOUDILFOUDILFOUDILFOUDILFOUDILFOUD"

  // Text[i] = (Crypted[i] - Key[i]) mod 30
  def decrypt() = {
    for (i <- 0 to Text.length() - 1) {
      val tChar = Text.charAt(i)
      val kChar = Key.charAt(i % Key.length())

      val tInt = Alpha.indexOf(tChar)
      val kInt = Alpha.indexOf(kChar)

      val cInt = (tInt - kInt + AlphaLen) % AlphaLen // +AlphaLen for positive range
      val cChar = Alpha.charAt(cInt)

      println(s"$i: $tChar - $kChar = $cChar")
      // print(cChar)
    }
  }

  def encrypt(text: String) = {
    for (i <- 0 to text.length() - 1) {
      val tChar = text.charAt(i)
      val kChar = Key.charAt(i % Key.length())

      val tInt = Alpha.indexOf(tChar)
      val kInt = Alpha.indexOf(kChar)

      val cInt = (tInt + kInt) % AlphaLen
      val cChar = Alpha.charAt(cInt)

      print(cChar)
    }
  }
}

object VigenerePuzzle {
  def main(args: Array[String]): Unit = {
    val secret = Vigenere.encrypt(Vigenere.Clear)
    println()
    val clear = Vigenere.decrypt()

    // println(s"Text=$clear")
  }
}
