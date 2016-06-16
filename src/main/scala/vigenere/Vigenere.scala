/* Algebraic Vigenere cypher */

object Vigenere {

  def hex2str(str: String): String =
    (for (c <- str.grouped(2)) yield {
      Integer.parseInt(c, 16).toChar
    }).mkString("")
}


class Vigenere(alpha: String) {

  def cryptOp(secret: String, key: String, op: (Int, Int) => Int): String = {
    (for ((s, k) <- (secret zip (Stream continually key).flatten)) yield {
      val si = alpha.indexOf(s)
      val ki = alpha.indexOf(k)

      val ci = op(si, ki)

      alpha.charAt(ci)
    }).mkString("")
  }

  def encrypt(secret: String, key: String) = {
    cryptOp(secret, key, (c: Int, k: Int) => (c + k) % alpha.length)
  }

  def decrypt(secret: String, key: String) = {
    val alen = alpha.length
    // +alen for correct range
    cryptOp(secret, key, (s: Int, k: Int) => (s - k + alen) % alen)
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
