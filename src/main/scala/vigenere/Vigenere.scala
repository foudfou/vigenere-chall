/* Algebraic Vigenere cypher */

object Vigenere {
  def hex2str(str: String): String = {
    def hex2strAux(str: String, res: String): String = {
      if (str.length == 0)
        res
      else if (str.length == 1)
        throw new IllegalArgumentException("Hex string not of even length")
      else {
        val char = Integer.parseInt(str.substring(0, 2), 16).toChar
        hex2strAux(str.substring(2), res + char)
      }
    }
    hex2strAux(str, "")
  }
}

class Vigenere(alpha: String) {

  def cryptOpStart(keyL: List[Char], secret: List[Char],
    op: (Int, Int) => Int): List[Char] = {

    def cryptOpAux(key: List[Char], sec: List[Char], clear: List[Char]):
        List[Char] = sec match {
      case Nil => clear
      case s :: ss =>
        val k = key.head

        val si = alpha.indexOf(s)
        val ki = alpha.indexOf(k)

        val ci = op(si, ki)
        val c = alpha.charAt(ci)

        cryptOpAux(key.tail, ss, c :: clear)
    }

    cryptOpAux(keyL, secret, List())
  }

  def decrypt(secret: String, key: String) = {
    val keyLong = for { i <- 0 to secret.length } yield key.charAt(i % key.length)

    val alen = alpha.length
    def decryptOp(s: Int, k: Int) = (s - k + alen) % alen // +alen for positive range

    // TODO: for (x <- xs; y <- ys) yield (x,y)
    val clearList = cryptOpStart(keyLong.toList, secret.toList, decryptOp)
    clearList.reverse.mkString("")
  }
}

object VigenerePuzzle {
  val Alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ. :/"
  val Key   = "0x464F5544494C"
  val Text  = "NLUPF/TLWRWWCVJWX/DNTJQAMEV/KZRNZR:OK:KC"

  def main(args: Array[String]): Unit = {
    val keyStr = Vigenere.hex2str(Key.stripPrefix("0x"))

    val vig = new Vigenere(Alpha)
    val clear = vig.decrypt(Text, keyStr)
    println(s"Text=$clear")

    // val secret = Vigenere.encrypt(Vigenere.Clear)
    // println()
  }
}
