package punycode

class SpecTests extends munit.FunSuite {
  test("encode.manual") {
    assertEquals(Punycode.encode("-"), "--")
    assertEquals(Punycode.encode(""), "")
    assertEquals(Punycode.encode("This has spaces"), "This has spaces-")
    assertEquals(Punycode.encode("London"), "London-")
    assertEquals(Punycode.encode("München"), "Mnchen-3ya")
    assertEquals(Punycode.encode("abæcdöef"), "abcdef-qua4k")
  }

  test("encode.traces") {
    assertEquals(
      Punycode.encode(deHex("4ED6 4EEC 4E3A 4EC0 4E48 4E0D 8BF4 4E2D 6587")),
      "ihqwcrb4cv8a8dqg056pqjye"
    )

    assertEquals(
      Punycode.encode(
        deHex(
          "0033 5E74 0042 7D44 91D1 516B 5148 751F"
        )
      ),
      "3B-ww4c5e180e575a65lsy2b"
    )

    assertEquals(
      Punycode.encode(deUHex("""
        u+0644 u+064A u+0647 u+0645 u+0627 u+0628 u+062A u+0643 u+0644
        u+0645 u+0648 u+0634 u+0639 u+0631 u+0628 u+064A u+061F
      """)),
      "egbpdaj6bu4bxfgehfvwxn"
    )

    assertEquals(
      Punycode.encode(deUHex("""
        u+4ED6 u+4EEC u+4E3A u+4EC0 u+4E48 u+4E0D u+8BF4 u+4E2D u+6587
      """)),
      "ihqwcrb4cv8a8dqg056pqjye"
    )

    assertEquals(
      Punycode.encode(deUHex("""
      U+0050 u+0072 u+006F u+010D u+0070 u+0072 u+006F u+0073 u+0074
      u+011B u+006E u+0065 u+006D u+006C u+0075 u+0076 u+00ED u+010D
      u+0065 u+0073 u+006B u+0079""")),
      "Proprostnemluvesky-uyb24dma41a"
    )

    assertEquals(
      Punycode.encode(deUHex("""
        U+0048 u+0065 u+006C u+006C u+006F u+002D U+0041 u+006E u+006F
        u+0074 u+0068 u+0065 u+0072 u+002D U+0057 u+0061 u+0079 u+002D
        u+305D u+308C u+305E u+308C u+306E u+5834 u+6240""")),
      "Hello-Another-Way--fc4qua05auwb3674vfr0b"
    )

    assertEquals(
      Punycode.encode(deUHex("""
      u+5B89 u+5BA4 u+5948 u+7F8E u+6075 u+002D u+0077 u+0069 u+0074
      u+0068 u+002D U+0053 U+0055 U+0050 U+0045 U+0052 u+002D U+004D
      U+004F U+004E U+004B U+0045 U+0059 U+0053""")),
      "-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n"
    )
  }

  test("decode.manual") {
    assertEquals(
      Punycode.decode("Mnchen-3ya"),
      "München"
    )

    assertEquals(Punycode.decode("abcdef-qua4k"), "abæcdöef")
    assertEquals(
      Punycode.decode("Bahnhof Mnchen-Ost-u6b"),
      "Bahnhof München-Ost"
    )

    assertEquals(
      Punycode.decode("MajiKoi5-783gue6qz075azm5e"),
      "MajiでKoiする5秒前"
    )

  }

  def deHex(hex: String): String = {
    new String(hex.split(" ").map(Integer.parseInt(_, 16).toChar))
  }

  def deUHex(uhex: String) = {
    val rgx = raw"[uU]\+([a-fA-F0-9]{4})".r
    val hex = rgx.findAllIn(uhex).toList.map(_.drop(2)).mkString(" ")
    deHex(hex)
  }

}
