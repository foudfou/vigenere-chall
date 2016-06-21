package vigenere

import org.scalatest.FunSuite


class VigenereSuite extends FunSuite {

  val Alpha  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ. :/"
  val KeyHex = "0x464F5544494C"
  val KeyStr = "FOUDIL"
  val Secret = "NLUPF/TLWRWWCVJWX/DNTJQAMEV/KZRNZR:OK:KC"
  val Clear  = "I AM SO COOL HTTPS://GITHUB.COM/FOUDFOU/"

  test("hex2key") {
    assert(Vigenere.hex2str(KeyHex.stripPrefix("0x")) == KeyStr)
  }

  test("Vigenere Algebraic encrypt") {
    val vig = new VigenereAlgebra(Alpha)
    assert(vig.encrypt(Clear, KeyStr) == Secret)
  }

  test("Vigenere Algebraic decrypt") {
    val vig = new VigenereAlgebra(Alpha)
    assert(vig.decrypt(Secret, KeyStr) == Clear)
  }

  test("Vigenere Tabula encrypt") {
    val vig = new VigenereTabula(Alpha)
    assert(vig.encrypt(Clear, KeyStr) == Secret)
  }

  test("Vigenere Tabula decrypt") {
    val vig = new VigenereTabula(Alpha)
    assert(vig.decrypt(Secret, KeyStr) == Clear)
  }

}
