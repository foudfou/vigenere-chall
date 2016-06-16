/* Algebraic Vigenere cypher */

object Vigenere {
  def hex2str(str: String): String = {
    if ((str.length % 2) != 0)
      throw new IllegalArgumentException("Hex string not of even length")

    def hex2strAux(str: String, res: String): String = {
      if (str.length == 0)
        res
      else {
        val char = Integer.parseInt(str.substring(0, 2), 16).toChar
        hex2strAux(str.substring(2), res + char)
      }
    }
    hex2strAux(str, "")
  }
}

class Vigenere(alpha: String) {

  def cryptOp(secret: String, key: String, op: (Int, Int) => Int): String = {
    val keyLong = for { i <- 0 to secret.length } yield key.charAt(i % key.length)

    (for ( (s, k) <- (secret zip keyLong)) yield {
      val si = alpha.indexOf(s)
      val ki = alpha.indexOf(k)

      val ci = op(si, ki)

      alpha.charAt(ci)
    }).mkString("")
  }

  def encrypt(secret: String, key: String) = {
    val alen = alpha.length
    def decryptOp(c: Int, k: Int) = (c + k) % alen
    cryptOp(secret, key, decryptOp)
  }

  def decrypt(secret: String, key: String) = {
    val alen = alpha.length
    def decryptOp(s: Int, k: Int) = (s - k + alen) % alen // +alen for positive range
    cryptOp(secret, key, decryptOp)
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
    println(clear)
    println(vig.decrypt(vig.encrypt(clear, keyStr), keyStr))
  }
}
