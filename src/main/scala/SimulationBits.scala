/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationBits(val width: Int) {
  val highest = width - 1
  val bits = new Array[Boolean](width)

  def :=(that: SimulationBits): SimulationBits = {
    that.bits.copyToArray(bits)
    that
  }

  def apply(idx: Int): Boolean = bits(idx)
  def update(idx: Int, bit: Boolean): Boolean = {bits(idx) = bit; bit}

  def int: Int = {
    var num: Int = 0
    var sig: Int = 1
    for (i <- 0 to highest) {
      if (bits(i)) {
        num |= sig
      }
      sig <<= 1
    }
    num
  }

  def sInt: Int = {
    var num: Int = 0
    var sig: Int = 1
    for (i <- 0 to highest) {
      if (bits(i)) {
        num |= sig
      }
      sig <<= 1
    }
    for (i <- highest + 1 to 31) {
      if (bits(highest)) {
        num |= sig
      }
      sig <<= 1
    }
    num
  }

  // Not suitable for assignment to more than 32 bits
  def int_=(num: Int): Int = {
    var sig: Int = 1
    for (i <- 0 to highest) {
      bits(i) = (num & sig) != 0
      sig <<= 1
    }
    num
  }

  def long: Long = {
    var num: Long = 0
    var sig: Long = 1
    for (i <- 0 to highest) {
      if (bits(i)) {
        num |= sig
      }
      sig <<= 1
    }
    num
  }

  // Not suitable for assignment to more than 64 bits
  def long_=(num: Long): Long = {
    var sig: Long = 1
    for (i <- 0 to highest) {
      bits(i) = (num & sig) != 0
      sig <<= 1
    }
    num
  }

  def sLong: Long = {
    var num: Long = 0
    var sig: Long = 1
    for (i <- 0 to highest) {
      if (bits(i)) {
        num |= sig
      }
      sig <<= 1
    }
    for (i <- highest + 1 to 63) {
      if (bits(highest)) {
        num |= sig
      }
      sig <<= 1
    }
    num
  }

  // Unsigned
  def bigInt: BigInt = {
    var num: BigInt = 0
    var sig: BigInt = 1
    for (i <- 0 to highest) {
      if (bits(i)) {
        num |= sig
      }
      sig <<= 1
    }
    num
  }

  // Signed
  def sBigInt: BigInt = {
    var num: BigInt = 0
    var sig: BigInt = 1
    if (bits(highest)) {
      num = -1 // Preset all bits to 1
      for (i <- 0 to highest) {
        if (!bits(i)) {
          num &~= sig
        }
        sig <<= 1
      }
    } else {
      num = 0
      for (i <- 0 to highest) {
        if (bits(i)) {
          num |= sig
        }
        sig <<= 1
      }
    }
    num
  }

  def bigInt_=(num: BigInt): BigInt = {
    var sig: BigInt = 1
    for (i <- 0 to highest) {
      bits(i) = (num & sig) != 0
      sig <<= 1
    }
    num
  }

  def float: Float = {
    java.lang.Float.intBitsToFloat(int)
  }

  def float_=(num: Float): Float = {
    int = java.lang.Float.floatToIntBits(num)
    num
  }

  def double: Double = {
    java.lang.Double.longBitsToDouble(long)
  }

  def double_=(num: Double): Double = {
    long = java.lang.Double.doubleToLongBits(num)
    num
  }

  override def toString(): String = {
    val str = new StringBuilder()
    for (i <- highest to 0 by -1) {
      str.append(if (bits(i)) "1" else "0")
    }
    str.toString()
  }
}
