/*
 Copyright Ashley Newson 2016
 */

package Chisel

import java.util.Base64
import collection.mutable.Set

trait SimulationAnnotation {
  def appendBase64FromBits(builder: StringBuilder, width: Int, n: Int, fn: (Int, Int) => Boolean): Unit = {
    var encodingBit: Int = 0
    var binData = Array[Byte](0, 0, 0)
    val base64enc = Base64.getEncoder()
    for (word <- 0 until n) {
      for (bit <- 0 until width) {
        if (fn(word, width - bit - 1)) {
          binData(encodingBit / 8) = (binData(encodingBit / 8) |  (1 << (encodingBit % 8))).toByte
        } else {
          binData(encodingBit / 8) = (binData(encodingBit / 8) & ~(1 << (encodingBit % 8))).toByte
        }
        encodingBit += 1
        if (encodingBit == 24) {
          builder.append(base64enc.encodeToString(binData))
          encodingBit = 0
        }
      }
    }
    if (encodingBit > 0) {
      for (bit <- encodingBit until 24) {
        binData(encodingBit / 8) = (binData(encodingBit / 8) & ~(1 << (encodingBit % 8))).toByte
      }
      builder.append(base64enc.encodeToString(binData.slice(0, (encodingBit + 7) / 8)))
    }
  }

  def isInSlice(sliceBits: Set[SimulationBit]): Boolean

  def isHidden: Boolean

  def dumpJSON(sliceBits: Set[SimulationBit]): String
}
