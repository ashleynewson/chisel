/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.{Set}

object SimulationBit {
  implicit def toBoolean(bit: SimulationBit): Boolean = bit.value
}

class SimulationBit(var value: Boolean, var producer: SimulationNode = null) {
  var criticalInputs = new AccumulatorSet[SimulationBit]()
  var inSlice = true

  def :=(that: Boolean) {
    value = that
    if (Driver.traceSimulation) {
      clear()
    }
  }

  def :=(that: SimulationBit) {
    value = that.value
    if (Driver.traceSimulation) {
      clear()
      criticalInputs ++= that.criticalInputs
      criticalInputs += that
    }
  }

  def depend(input: SimulationBit) {
    if (Driver.traceSimulation) {
      criticalInputs ++= input.criticalInputs
      criticalInputs += input
    }
  }

  def unfreeze_trace() {
    criticalInputs.unpreserve()
  }

  def freeze_trace() {
    criticalInputs.preserve()
  }

  def clear() {
    // criticalInputs.clear()
    if (Driver.traceSimulation) {
      criticalInputs.freeze()
      criticalInputs.collapse()
      criticalInputs = new AccumulatorSet[SimulationBit]()
    }
  }

  def affectedBy(bit: SimulationBit): Boolean = {
    criticalInputs.contains(bit)
  }

  // def critical: Boolean = {criticalFlag}
  // def critical_=(that: Boolean): Unit = {criticalFlag = forwardTrack || that}
}
