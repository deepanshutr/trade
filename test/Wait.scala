package desk

object Wait:
  def until(label: String, seconds: Double = 5)(condition: => Boolean): Unit =
    val deadline = System.nanoTime() + (seconds * 1e9).toLong
    while !condition && System.nanoTime() < deadline do Thread.sleep(2)
    assert(condition, label)
