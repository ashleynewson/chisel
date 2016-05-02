/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.Set

class SimulationROMData(node: ROMData) extends SimulationNode(node) {
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

  override def isInSlice(sliceBits: Set[SimulationBit]): Boolean = {
    if (outputBits.isInSlice(sliceBits)) {
      return true
    } else {
      for (word <- data) {
        if (outputBits.isInSlice(sliceBits)) {
          return true
        }
      }
    }
    return false
  }

  override def dumpJSON(sliceBits: Set[SimulationBit]): String = {
    val builder = new StringBuilder()
    builder.append("{")
    builder.append("\"name\":\"" + node.annotationName + "\",")
    builder.append("\"type\":\"data\",")
    builder.append("\"in\":" + isInSlice(sliceBits) + ",")
    builder.append("\"hide\":" + isHidden + ",")
    builder.append("\"width\":" + node.width + ",")
    builder.append("\"size\":" + node.n + ",")

    {
      builder.append("\"state\":\"")
      appendBase64FromBits(builder, node.width, node.n, (word: Int, bit: Int) => {data(word)(bit)})
      builder.append("\",")
    }
    {
      builder.append("\"mask\":\"")
      appendBase64FromBits(builder, node.width, node.n, (word: Int, bit: Int) => {sliceBits.contains(data(word)(bit))})
      builder.append("\",")
    }

    builder.append("}")
    builder.toString
  }
}
