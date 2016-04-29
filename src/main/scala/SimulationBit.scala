/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.{Set}

object SimulationBit {
  implicit def toBoolean(bit: SimulationBit): Boolean = bit.value
}

class SimulationBit(var value: Boolean, var producer: SimulationNode) {
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

  /* Clear dependencies for this node */
  def clear() {
    // criticalInputs.clear()
    if (Driver.traceSimulation) {
      criticalInputs.freeze()
      criticalInputs.collapse()
      criticalInputs = new AccumulatorSet[SimulationBit]()
    }
  }

  /* If we want to add dependencies after the current set of
   * dependencies have been passed on, we need to be told so. */
  def checkpoint(selfInclude: Boolean): Unit = {
    if (Driver.traceSimulation) {
      val oldSet = criticalInputs
      criticalInputs = new AccumulatorSet[SimulationBit]()
      criticalInputs ++= oldSet
      if (selfInclude) {
        criticalInputs += this
      }
      oldSet.freeze()
      oldSet.collapse()
    }
  }

  def affectedBy(bit: SimulationBit): Boolean = {
    if (criticalInputs.contains(bit)) {
      true
    } else if (producer != null) {
      producer.extra_dependence(this).contains(bit)
    } else {
      false
    }
  }

  // def critical: Boolean = {criticalFlag}
  // def critical_=(that: Boolean): Unit = {criticalFlag = forwardTrack || that}
}
