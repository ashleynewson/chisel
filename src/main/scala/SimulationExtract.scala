/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationExtract(node: Extract) extends SimulationNode(node) {
  override val clocked = false

  // Need to check dependence validity
  override def evaluate(): Unit = {
    // Can the lo and hi be non constant?
    val hi = inputs(1).output.int
    val lo = inputs(2).output.int
    for (i <- lo to hi) {
      outputBits(i - lo) := inputs(0).output(i)
    }
  }
}
