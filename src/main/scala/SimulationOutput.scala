/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationOutput(node: Data) extends SimulationNode(node) {
  override val clocked = false

  override def evaluate(): Unit = {
    outputBits := inputs(0).output
    // System.err.println("Output " + node.line.getFileName() + ":" + node.line.getLineNumber() + " is " + output.toString())
  }

  override def annotationName: String = {"IO " + inputsString}
}
