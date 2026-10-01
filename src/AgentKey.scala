package desk

import java.nio.charset.StandardCharsets.US_ASCII
import java.security.{MessageDigest, SecureRandom}
import org.bouncycastle.crypto.generators.Argon2BytesGenerator
import org.bouncycastle.crypto.params.Argon2Parameters

// A random 256 bit seed written as 13 groups of five characters. Four characters of each group
// carry data and the fifth checks them. Every buffer here is a byte array the caller zeroes.
object SeedText:
  private val groupCount = 13
  val groups: Int = groupCount

  private val alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ".getBytes(US_ASCII)
  private val prefix = "desk-seed-v1".getBytes(US_ASCII)
  private val dash = '-'.toByte
  private val space = ' '.toByte

  private def wipe(bytes: Array[Byte]): Unit = java.util.Arrays.fill(bytes, 0.toByte)

  // The value of each alphabet byte, or -1 for any other byte below 128.
  private val values: Array[Int] =
    val table = Array.fill(128)(-1)
    alphabet.zipWithIndex.foreach((byte, index) => table(byte) = index)
    table

  private def value(byte: Byte): Int = if byte < 0 then -1 else values(byte)

  private def bit(seed: Array[Byte], position: Int): Int =
    if position >= 256 then 0 else (seed(position >> 3) >> (7 - (position & 7))) & 1

  private def character(seed: Array[Byte], index: Int): Int =
    var result = 0
    var offset = 0
    while offset < 5 do
      result = (result << 1) | bit(seed, index * 5 + offset)
      offset += 1
    result

  private def check(index: Int, buffer: Array[Byte], offset: Int): Int =
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(prefix)
    digest.update(index.toByte)
    digest.update(buffer, offset, 4)
    val hash = digest.digest()
    try (hash(0) & 0xff) >> 3
    finally wipe(hash)

  def encode(seed: Array[Byte]): Array[Byte] =
    if seed == null || seed.length != 32 then throw new IllegalArgumentException("Unsupported seed")
    val text = new Array[Byte](groupCount * 6 - 1)
    var done = false
    try
      var group = 0
      while group < groupCount do
        val start = group * 6
        if group > 0 then text(start - 1) = dash
        var position = 0
        while position < 4 do
          text(start + position) = alphabet(character(seed, group * 4 + position))
          position += 1
        text(start + 4) = alphabet(check(group, text, start))
        group += 1
      done = true
      text
    finally if !done then wipe(text)

  def accept(byte: Byte): Option[Byte] =
    val raw = byte & 0xff
    val upper = if raw >= 'a'.toInt && raw <= 'z'.toInt then raw - 32 else raw
    val mapped: Int =
      if upper == 'I'.toInt || upper == 'L'.toInt then '1'.toInt
      else if upper == 'O'.toInt then '0'.toInt
      else upper
    if mapped < 128 && values(mapped) >= 0 then Some(mapped.toByte) else None

  def refused(byte: Byte): Boolean = accept(byte).isEmpty && byte != dash && byte != space

  private def groupValidAt(index: Int, buffer: Array[Byte], offset: Int): Boolean =
    index >= 0 && index < groupCount && (0 until 5).forall(i => value(buffer(offset + i)) >= 0) &&
      value(buffer(offset + 4)) == check(index, buffer, offset) &&
      (index != groupCount - 1 || (value(buffer(offset + 3)) & 15) == 0)

  def groupValid(index: Int, group: Array[Byte]): Boolean =
    group != null && group.length == 5 && groupValidAt(index, group, 0)

  def decode(groups: Array[Byte]): Option[Array[Byte]] =
    if groups == null || groups.length != groupCount * 5 then None
    else if !(0 until groupCount).forall(index => groupValidAt(index, groups, index * 5)) then None
    else
      val seed = new Array[Byte](32)
      var index = 0
      while index < groupCount * 4 do
        val character = value(groups((index / 4) * 5 + index % 4))
        var offset = 0
        while offset < 5 do
          val position = index * 5 + offset
          if position < 256 then
            val bit = (character >> (4 - offset)) & 1
            seed(position >> 3) = (seed(position >> 3) | (bit << (7 - (position & 7)))).toByte
          offset += 1
        index += 1
      Some(seed)

// The trading key made from a seed and a passphrase. The caller owns and zeroes every array it
// passes in or gets back. A passphrase buffer is fixed and length says how much of it is in use.
object AgentKey:
  private val saltPrefix = "desk-agent-v1".getBytes(US_ASCII)
  private val memoryCost = 262144
  private val passes = 3
  private val parallelism = 4
  // The secp256k1 group order, as bytes so a key never passes through a BigInteger.
  private val order = java.util.HexFormat
    .of()
    .parseHex("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141")

  def newSeed(random: SecureRandom): Array[Byte] =
    val seed = new Array[Byte](32)
    random.nextBytes(seed)
    seed

  def passphraseValid(passphrase: Array[Byte], length: Int): Boolean =
    passphrase != null && length >= 8 && length <= 128 && length <= passphrase.length &&
      (0 until length).forall(index => passphrase(index) >= 32 && passphrase(index) <= 126)

  private def wipe(bytes: Array[Byte]): Unit = java.util.Arrays.fill(bytes, 0.toByte)

  private def unsupported(): Nothing =
    throw new IllegalArgumentException("Unsupported key input")

  def derive(seed: Array[Byte], passphrase: Array[Byte], length: Int): Array[Byte] =
    deriveObserved(seed, passphrase, length)((_, _) => ())

  // Hands each buffer that is zeroed afterwards to observe, so a test can check the wipe.
  private[desk] def deriveObserved(seed: Array[Byte], passphrase: Array[Byte], length: Int)(
      observe: (String, Array[Byte]) => Unit
  ): Array[Byte] =
    if seed == null || seed.length != 32 || !passphraseValid(passphrase, length) then unsupported()
    val salt = new Array[Byte](saltPrefix.length + 1 + length)
    val output = new Array[Byte](32)
    try
      System.arraycopy(saltPrefix, 0, salt, 0, saltPrefix.length)
      System.arraycopy(passphrase, 0, salt, saltPrefix.length + 1, length)
      observe("salt", salt)
      argon2id(
        seed,
        salt,
        memoryCost,
        passes,
        parallelism,
        output,
        Array.emptyByteArray,
        Array.emptyByteArray
      )
      observe("output", output)
      reduceWith(output, sha256, observe)
    finally
      wipe(salt)
      wipe(output)

  // The one BouncyCastle call. Desk gives it no secret and no associated data.
  private[desk] def argon2id(
      password: Array[Byte],
      salt: Array[Byte],
      memoryKiB: Int,
      iterations: Int,
      lanes: Int,
      output: Array[Byte],
      secret: Array[Byte],
      additional: Array[Byte]
  ): Unit =
    val builder = new Argon2Parameters.Builder(Argon2Parameters.ARGON2_id)
      .withVersion(Argon2Parameters.ARGON2_VERSION_13)
      .withMemoryAsKB(memoryKiB)
      .withIterations(iterations)
      .withParallelism(lanes)
      .withSalt(salt)
    if secret.length > 0 then builder.withSecret(secret)
    if additional.length > 0 then builder.withAdditional(additional)
    val parameters = builder.build()
    try
      val generator = new Argon2BytesGenerator
      generator.init(parameters)
      val _ = generator.generateBytes(password, output)
    finally
      builder.clear()
      parameters.clear()

  private def sha256(input: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(input)

  private def inRange(value: Array[Byte]): Boolean =
    var first = 0
    while first < 32 && value(first) == order(first) do first += 1
    val below = first < 32 && (value(first) & 0xff) < (order(first) & 0xff)
    below && value.exists(_ != 0)

  private[desk] def reduce(output: Array[Byte]): Array[Byte] =
    reduceWith(output, sha256, (_, _) => ())

  // A value in range comes back as a copy. Otherwise try the hash of the value and a counter
  // from 1 and use the first in range. Hands each buffer that is zeroed afterwards to observe.
  private[desk] def reduceWith(
      output: Array[Byte],
      hash: Array[Byte] => Array[Byte],
      observe: (String, Array[Byte]) => Unit
  ): Array[Byte] =
    if output == null || output.length != 32 then unsupported()
    if inRange(output) then output.clone()
    else
      val input = new Array[Byte](36)
      try
        System.arraycopy(output, 0, input, 0, 32)
        var counter = 1
        var found: Array[Byte] = null
        while found == null do
          input(32) = (counter >>> 24).toByte
          input(33) = (counter >>> 16).toByte
          input(34) = (counter >>> 8).toByte
          input(35) = counter.toByte
          observe("counter input", input)
          val candidate = hash(input)
          if inRange(candidate) then found = candidate
          else
            observe("rejected candidate", candidate)
            wipe(candidate)
            counter += 1
        found
      finally wipe(input)

  def approvalName(address: String): String =
    if address == null || !address.matches("0x[0-9a-fA-F]{40}") then
      throw new IllegalArgumentException("Unsupported address")
    "desk-" + address.substring(2, 12).toLowerCase(java.util.Locale.ROOT)
