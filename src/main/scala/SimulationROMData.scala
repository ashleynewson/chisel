/*
  Copyright Ashley Newson 2016
*/

package Chisel

import java.util.Base64
import collection.mutable.Set

class SimulationROMData(node: ROMData) extends SimulationNode(node) with SimulationAnnotation {
  override val clocked = false
  val size = node.n

  val data = new Array[SimulationBits](size)
  for (i <- 0 to size-1) {
    data(i) = new SimulationBits(node.width)
    data(i).bigInt = if (node.sparseLits.contains(i)) node.sparseLits(i).value else 0
  }

  override def getSimulationBits(): Set[SimulationBit] = {
    val bits = Set[SimulationBit]()
    bits ++= output.bits.toSet
    for (datum <- data) {
      bits ++= datum.bits.toSet
    }
    bits
  }

  override def evaluate(): Unit = {
  }

  def read(addr: Int): SimulationBits = {
    data(addr)
  }

  def dumpJSON(sliceBits: Set[SimulationBit]): String = {
    val builder = new StringBuilder()
    builder.append("{")
    builder.append("\"type\":\"mem\",")
    builder.append("\"width\":" + node.width + ",")
    builder.append("\"size\":" + node.n + ",")
    def appendBase64FromBits(width: Int, n: Int, fn: (Int, Int) => Boolean): Unit = {
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
    {
      builder.append("\"state\":\"")
      appendBase64FromBits(node.width, node.n, (word: Int, bit: Int) => {data(word)(bit)})
      builder.append("\",")
    }
    {
      builder.append("\"mask\":\"")
      appendBase64FromBits(node.width, node.n, (word: Int, bit: Int) => {sliceBits.contains(data(word)(bit))})
      builder.append("\",")
    }
    builder.append("}")
    builder.toString
  }
}
