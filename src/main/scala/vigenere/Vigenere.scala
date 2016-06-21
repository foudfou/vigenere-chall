package vigenere


object Vigenere {
  def hex2str(str: String): String =
    (for (c <- str.grouped(2)) yield Integer.parseInt(c, 16).toChar).mkString
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
    }).mkString
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
    }).mkString
  }
}
