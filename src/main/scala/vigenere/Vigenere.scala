/* Algebraic Vigenere cypher */

object Vigenere {

  def hex2str(str: String): String =
    (for (c <- str.grouped(2)) yield {
      Integer.parseInt(c, 16).toChar
    }).mkString("")
}


class VigenereTabula(alpha: String) {
  val tabula = for (i <- 0 to alpha.length - 1) yield
    (i to alpha.length - 1) ++ (0 to i - 1)
  val alphaRevMap = (for (i <- alpha.indices) yield alpha(i) -> i).toMap

  def encrypt(clear: String, key: String) =
    cryptOp(clear, key,
      (c, k) => tabula(alphaRevMap(k))(alphaRevMap(c)))

  def decrypt(secret: String, key: String) =
    cryptOp(secret, key,
      (s, k) => tabula(alphaRevMap(k)).indexOf(alphaRevMap(s)))

  def cryptOp(text: String, key: String, op: (Char, Char) => Int) =
    (for ((t, k) <- (text zip (Stream continually key).flatten)) yield {
      alpha(op(t, k))
    }).mkString("")
}

class VigenereAlgebra(alpha: String) {
  def encrypt(secret: String, key: String) = {
    cryptOp(secret, key, (c: Int, k: Int) => (c + k) % alpha.length)
  }

  def decrypt(secret: String, key: String) = {
    val alen = alpha.length
    // +alen for correct range
    cryptOp(secret, key, (s: Int, k: Int) => (s - k + alen) % alen)
  }

  def cryptOp(secret: String, key: String, op: (Int, Int) => Int): String = {
    (for ((s, k) <- (secret zip (Stream continually key).flatten)) yield {
      val si = alpha.indexOf(s)
      val ki = alpha.indexOf(k)

      val ci = op(si, ki)

      alpha.charAt(ci)
    }).mkString("")
  }
}

object VigenereChallenge {
  val Alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ. :/"
  val Key   = "0x464F5544494C"
  val Text  = "NLUPF/TLWRWWCVJWX/DNTJQAMEV/KZRNZR:OK:KC"

  def main(args: Array[String]): Unit = {
    val keyStr = Vigenere.hex2str(Key.stripPrefix("0x"))

    val vigAlg = new VigenereAlgebra(Alpha)
    val clearAlg = vigAlg.decrypt(Text, keyStr)
    println(clearAlg)
    println(vigAlg.decrypt(vigAlg.encrypt(clearAlg, keyStr), keyStr))

    val vigTab = new VigenereTabula(Alpha)
    val clearTab = vigTab.decrypt(Text, keyStr)
    println(vigTab.decrypt(Text, keyStr))
    println(vigTab.encrypt(clearTab, keyStr))
  }
}
