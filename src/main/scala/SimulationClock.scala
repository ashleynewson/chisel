/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.Set

class SimulationClock(val clock: Clock) extends SimulationNode(clock) {
  override val clocked = false
  val period: Long = clock.period.toLong
  var remaining: Long = 0
  val userSet = Set[SimulationNode]()
  var users = List[SimulationNode]()

  /**
   * Add a node which uses (directly or indirectly) this clock).
   */
  def addUser(user: SimulationNode) = {
    userSet += user
  }

  def orderSimulationNodes(): Unit = {
    users = userSet.toList.sortWith(_ < _)
  }

  def step(): Unit = {
    for (simulationNode <- users) {
      simulationNode.evaluate()
    }
  }

  override def evaluate(): Unit = {
   if (inputs(0).output(0)) {
      remaining = period
    }
  }
}
