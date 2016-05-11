/*
  Copyright Ashley Newson 2016
*/

package Chisel

import scala.collection.mutable.Set

class SimulationBits(val width: Int, var producer: SimulationNode = null) {
  val highest = width - 1
  val bits = new Array[SimulationBit](width)
  for (i <- 0 to highest) bits(i) = new SimulationBit(false, producer)
  // val bits = new Array[Boolean](width)
  // val history = new Array[SimulationBitHistory](width)

  def :=(that: SimulationBits): SimulationBits = {
    // that.bits.copyToArray(bits)
    // that.history.copyToArray(history)
    // that
    val limit = if (width < that.width) highest else that.width
    for (i <- 0 until limit) {
      bits(i) := that.bits(i)
    }
    for (i <- limit until width) {
      bits(i) := false
    }
    that
  }

  def apply(idx: Int): SimulationBit = {
    bits(idx)
  }
  def update(idx: Int, bit: SimulationBit): SimulationBit = {
    bits(idx) := bit
    bit
  }
  def update(idx: Int, bit: Boolean): Boolean = {
    bits(idx) := bit
    bit
  }

  def int: Int = {
    var num: Int = 0
    var sig: Int = 1
    for (i <- 0 to highest) {
      if (bits(i).value) {
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
      if (bits(i).value) {
        num |= sig
      }
      sig <<= 1
    }
    for (i <- highest + 1 to 31) {
      if (bits(highest).value) {
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
      bits(i).value = (num & sig) != 0
      sig <<= 1
    }
    num
  }

  def long: Long = {
    var num: Long = 0
    var sig: Long = 1
    for (i <- 0 to highest) {
      if (bits(i).value) {
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
      bits(i).value = (num & sig) != 0
      sig <<= 1
    }
    num
  }

  def sLong: Long = {
    var num: Long = 0
    var sig: Long = 1
    for (i <- 0 to highest) {
      if (bits(i).value) {
        num |= sig
      }
      sig <<= 1
    }
    for (i <- highest + 1 to 63) {
      if (bits(highest).value) {
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
      if (bits(i).value) {
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
    if (bits(highest).value) {
      num = -1 // Preset all bits to 1
      for (i <- 0 to highest) {
        if (!bits(i).value) {
          num &~= sig
        }
        sig <<= 1
      }
    } else {
      num = 0
      for (i <- 0 to highest) {
        if (bits(i).value) {
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
      bits(i).value = (num & sig) != 0
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
      str.append(if (bits(i).value) "1" else "0")
      // str.append(if (bits(i).critical) "*" else " ")
    }
    str.toString()
  }

  def clear(): Unit = {
    if (Driver.traceSimulation) {
      for (bit <- bits) {
        bit.clear()
      }
    }
  }

  def checkpoint(selfInclude: Boolean): Unit = {
    if (Driver.traceSimulation) {
      for (bit <- bits) {
        bit.checkpoint(selfInclude)
      }
    }
  }

  // def critical: Boolean = {
  //   for (bit <- bits) {
  //     if (bit.critical) {
  //       return true
  //     }
  //   }
  //   false
  // }

  def depend(that: SimulationBits): Unit = {
    if (Driver.traceSimulation) {
      val proxySet = new AccumulatorSet[SimulationBit]()
      for (tbit <- that.bits) {
        proxySet ++= tbit.criticalInputs
        proxySet += tbit
      }
      for (bit <- bits) {
        bit.criticalInputs ++= proxySet
      }
      proxySet.freeze()
      proxySet.collapse()
      // for (tbit <- that.bits) {
      //   // if (tbit.critical) {
      //   for (bit <- bits) {
      //     bit.depend(tbit)
      //   }
      //   // }
      // }
    }
  }

  def depend(that: SimulationTester.DependenceSet): Unit = {
      depend(that, (BigInt(1) << width) - 1)
  }

  def depend(that: SimulationTester.DependenceSet, mask: BigInt): Unit = {
    if (Driver.traceSimulation) {
      var submask = BigInt(1)
      for (i <- 0 until width) {
        if ((submask & mask) != 0) {
          bits(i).criticalInputs ++= that.set
        }
        submask <<= 1
      }
    }
  }

  // def criticalInputs: Set[SimulationNode] = {
  //   var nodes = Set[SimulationNode]()
  //   for (bit <- bits) {
  //     nodes ++= bit.criticalInputs
  //   }
  //   nodes
  // }

  def isInSlice(sliceBits: Set[SimulationBit]): Boolean = {
    for (bit <- bits) {
      if (sliceBits.contains(bit)) {
        return true
      }
    }
    return false
  }
}
