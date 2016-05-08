/*
  Copyright Ashley Newson 2016
*/

package Chisel

import scala.collection.mutable.Set

/** Used only as a way of tapping Data nodes */
class SimulationLiteral(node: Literal) extends SimulationNode(node) {
  override val clocked = false
  outputBits.long = node.value.toLong

  override def postLinkSetup(): Unit = {
    if (inputs.size > 0) {
      var baseLit = this
      while (baseLit.inputs.size > 0) {
        baseLit = baseLit.inputs(0).asInstanceOf[SimulationLiteral]
      }
      outputBits := baseLit.output
    }
  }

  override def clearDependencies(): Unit = {
    // Don't clear anything!
  }

  override def evaluate(): Unit = {
  }

  override def staticDumpJSON(sliceBits: Set[SimulationBit]): String = {
    val builder = new StringBuilder()
    builder.append("{")
    builder.append("\"name\":\"" + node.annotationName + "\",")
    builder.append("\"type\":\"data\",")
    builder.append("\"in\":" + isInSlice(sliceBits) + ",")
    builder.append("\"hide\":" + isHidden + ",")
    builder.append("\"width\":" + outputBits.width + ",")
    builder.append("\"size\":1,")

    {
      builder.append("\"state\":\"")
      appendBase64FromBits(builder, outputBits.width, 1, (word: Int, bit: Int) => {outputBits(bit)})
      builder.append("\",")
    }
    {
      builder.append("\"mask\":\"")
      appendBase64FromBits(builder, outputBits.width, 1, (word: Int, bit: Int) => {sliceBits.contains(outputBits(bit))})
      builder.append("\",")
    }

    builder.append("}")
    builder.toString
  }

  override def staticDependencies(bit: SimulationBit): Set[SimulationBit] = {
    if (inputs.size > 0) {
      staticDependencyBit(inputs(0), bit)
    } else {
      Set()
    }
  }

  override def staticDependents(bit: SimulationBit): Set[SimulationBit] = {
    staticDependentBit(inputs(0), bit)
  }
}
