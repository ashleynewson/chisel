/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.{Set}

object SimulationBit {
  implicit def toBoolean(bit: SimulationBit): Boolean = bit.value
}

class SimulationBit(var value: Boolean, var producer: SimulationNode = null) {
  var criticalInputs = Set[SimulationBit]()
  var inSlice = true

  def :=(that: Boolean) {
    value = that
    criticalInputs.clear()
  }

  def :=(that: SimulationBit) {
    value = that.value
    criticalInputs.clear()
    criticalInputs ++= that.criticalInputs
    criticalInputs += that
  }

  def depend(input: SimulationBit) {
    criticalInputs ++= input.criticalInputs
    criticalInputs += input
  }

  def clear() {
    criticalInputs.clear()
  }

  def affectedBy(bit: SimulationBit): Boolean = {
    criticalInputs.contains(bit)
  }

  // def critical: Boolean = {criticalFlag}
  // def critical_=(that: Boolean): Unit = {criticalFlag = forwardTrack || that}
}
