/* Algebraic Vigenere cypher */

object Vigenere {

  def decrypt(secret: String, key: String, alpha: String) = {
    val alen = alpha.length()
    def decryptAux(pos: Int, sec: List[Char], clear: List[Char]): List[Char] =
      sec match {
        case Nil => clear
        case s :: ss =>
          val k = key.charAt(pos % key.length())

          val si = alpha.indexOf(s)
          val ki = alpha.indexOf(k)

          val ci = (si - ki + alen) % alen // +alen for positive range
          val c = alpha.charAt(ci)

          decryptAux(pos + 1, ss, c :: clear)
      }
    val clearList = decryptAux(0, secret.toList, List())
    clearList.reverse.mkString("")
  }
}

object VigenerePuzzle {
  val Alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ. :/"
  val Key = "FOUDIL"  // 0x464F5544494C
  val Text  = "NLUPF/TLWRWWCVJWX/DNTJQAMEV/KZRNZR:OK:KC"
  val Clear = "I AM SO COOL HTTPS://GITHUB.COM/FOUDFOU/"
  //          "FOUDILFOUDILFOUDILFOUDILFOUDILFOUDILFOUD"

  def main(args: Array[String]): Unit = {
    val clear = Vigenere.decrypt(Text, Key, Alpha)
    println(s"Text=$clear")

    // val secret = Vigenere.encrypt(Vigenere.Clear)
    // println()
  }
}
