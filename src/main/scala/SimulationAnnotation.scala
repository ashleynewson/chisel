/*
 Copyright Ashley Newson 2016
 */

package Chisel

import collection.mutable.Set

trait SimulationAnnotation {
  def dumpJSON(sliceBits: Set[SimulationBit]): String
}
