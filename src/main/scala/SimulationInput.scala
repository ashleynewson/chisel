/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationInput(node: Data) extends SimulationNode(node) {
  override val clocked = true

  override def postLinkSetup(simulation: Simulation): Unit = {
    // This is so that inputs can be modified before every clock tick.
    // It may be more appropriate to use only the implicit clock?
    // Need to refer to specification.
    for (simulationClock <- simulation.simulationClocks) {
      addClock(simulationClock)
    }
  }

  override def evaluate(): Unit = {
    System.err.println("Input for " + node.line.getFileName() + ":" + node.line.getLineNumber())
    val value = readLine()
    if (value != null && value != "") {
        outputBits.long = value.toLong
    } else {
      System.err.println("Input preserved.")
    }
    System.err.println("Set " + node.line.getFileName() + ":" + node.line.getLineNumber() + " to " + output.toString())
  }
}
