/*
  Copyright Ashley Newson 2016
*/

package Chisel

/** Used only as a way of tapping Data nodes */
class SimulationBuffer(node: Node) extends SimulationNode(node) {
  override val clocked = false

  override def evaluate(): Unit = {
    // Yes, not ':=', but '='. It's just cheaper.
    outputBits = inputs(0).output
  }

  override def annotationName: String = {"Data " + inputsString}
}
