/*
 Copyright Ashley Newson 2016
 */

package Chisel

import collection.mutable.Set

class SimulationMem(node: Mem[_]) extends SimulationNode(node) {
  override val clocked = true

  val data = new Array[SimulationBits](node.n)
  for (i <- 0 to node.n-1) {
    data(i) = new SimulationBits(node.width, this)
  }

  override def getSimulationBits(): Set[SimulationBit] = {
    val bits = Set[SimulationBit]()
    bits ++= output.bits.toSet
    for (datum <- data) {
      bits ++= datum.bits.toSet
    }
    bits
  }

  // Need to revise the technicallity vs utility of this section.
  override def evaluate(): Unit = {
    for (input <- inputs) {
      val writer = input.asInstanceOf[SimulationMemWrite]

      if (writer.inputs(1).output(0)) {
        write(writer.inputs(0).output.int, writer.inputs(2).output)
        data(writer.inputs(0).output.int).depend(inputs(0).output)
        data(writer.inputs(0).output.int).depend(inputs(1).output)
      }
    }
  }

  // Need to revise the technicallity vs utility of this section.
  def write(addr: Int, datum: SimulationBits): Unit = {
    data(addr) := datum
  }

  def read(addr: Int): SimulationBits = {
    data(addr)
  }

  override def annotationName: String = {"RAM"}

  override def dumpJSON(sliceBits: Set[SimulationBit]): String = {
    val builder = new StringBuilder()
    builder.append("{")
    builder.append("\"name\":\"" + annotationName + "\",")
    builder.append("\"type\":\"data\",")
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
