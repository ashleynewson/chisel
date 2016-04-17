/*
  Copyright Ashley Newson 2016
*/

package Chisel

/** Used only as a way of tapping Data nodes */
class SimulationLiteral(node: Literal) extends SimulationNode(node) {
  override val clocked = false
  outputBits.long = node.value.toLong

  override def evaluate(): Unit = {
  }

  override def annotationName: String = {"Literal"}
}
