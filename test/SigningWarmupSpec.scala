package desk

import java.nio.file.{Files, Paths}

class SigningWarmupSpec extends munit.FunSuite:
  private val fixture = ujson.read(
    Files.readString(Paths.get("fixtures", "order_signing.json"))
  )("vectors")(2)
  private val dummyKey = java.util.HexFormat
    .of()
    .parseHex(
      "0123456789012345678901234567890123456789012345678901234567890123"
    )

  test("preparation signs one published dummy fixture and clears its key") {
    var calls = 0
    var retained = Option.empty[Array[Byte]]
    OrderSigning.warm((key, action, nonce, mainnet, expiry) =>
      calls += 1
      retained = Some(key)
      assert(key.sameElements(dummyKey))
      assertEquals(action, fixture("action"))
      assertEquals(nonce, fixture("nonce").num.toLong)
      assertEquals(mainnet, false)
      assertEquals(expiry, Some(fixture("expiresAfter").num.toLong))
      val signature = OrderSigning.sign(key, action, nonce, mainnet, expiry)
      assertEquals(signature, fixture("signature"))
      signature
    )
    assertEquals(calls, 1)
    assert(retained.exists(_.forall(_ == 0)))
  }

  test("preparation clears its dummy key when signing fails") {
    var retained = Option.empty[Array[Byte]]
    val failure = new IllegalStateException("test signing failure")
    val thrown = intercept[IllegalStateException] {
      OrderSigning.warm((key, _, _, _, _) =>
        retained = Some(key)
        throw failure
      )
    }
    assert(thrown eq failure)
    assert(retained.exists(_.forall(_ == 0)))
  }
