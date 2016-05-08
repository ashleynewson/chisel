/*
  Copyright Ashley Newson 2016
*/

package Chisel

import scala.collection.mutable.Set

class SimulationOp(node: Op) extends SimulationNode(node) {
  override val clocked = false

  override def evaluate(): Unit = {
    outputBits.clear()
    inputs.length match {
      case 1 =>
        node.op match {
          case "~" =>
            for (i <- 0 to outputBits.highest) {
              outputBits(i) := !inputs(0).output(i)
              outputBits(i).depend(inputs(0).output(i))
            }
          case "f-" => {
            outputBits := inputs(0).output
            outputBits(31) := !inputs(0).output(31)
            outputBits(31).depend(inputs(0).output(31))
          }
          case "fsin" => {
            outputBits.float = math.sin(inputs(0).output.float.toDouble).toFloat
            outputBits.depend(inputs(0).output)
          }
          case "fcos" => {
            outputBits.float = math.cos(inputs(0).output.float.toDouble).toFloat
            outputBits.depend(inputs(0).output)
          }
          case "ftan" => {
            outputBits.float = math.tan(inputs(0).output.float.toDouble).toFloat
            outputBits.depend(inputs(0).output)
          }
          case "fsqrt" => {
            outputBits.float = math.sqrt(inputs(0).output.float.toDouble).toFloat
            outputBits.depend(inputs(0).output)
          }
          case "flog" => {
            outputBits.float = math.log(inputs(0).output.float.toDouble).toFloat
            outputBits.depend(inputs(0).output)
          }
          case "ffloor" => {
            outputBits.float = inputs(0).output.float.floor
            outputBits.depend(inputs(0).output)
          }
          case "fceil" => {
            outputBits.float = inputs(0).output.float.ceil
            outputBits.depend(inputs(0).output)
          }
          case "fround" => {
            outputBits.float = inputs(0).output.float.round
            outputBits.depend(inputs(0).output)
          }
          case "d-" => {
            outputBits := inputs(0).output
            outputBits(63) := !inputs(0).output(63)
            outputBits(63).depend(inputs(0).output(63))
          }
          case "dsin" => {
            outputBits.double = math.sin(inputs(0).output.double)
            outputBits.depend(inputs(0).output)
          }
          case "dcos" => {
            outputBits.double = math.cos(inputs(0).output.double)
            outputBits.depend(inputs(0).output)
          }
          case "dtan" => {
            outputBits.double = math.tan(inputs(0).output.double)
            outputBits.depend(inputs(0).output)
          }
          case "dsqrt" => {
            outputBits.double = math.sqrt(inputs(0).output.double)
            outputBits.depend(inputs(0).output)
          }
          case "dlog" => {
            outputBits.double = math.log(inputs(0).output.double)
            outputBits.depend(inputs(0).output)
          }
          case "dfloor" => {
            outputBits.double = inputs(0).output.double.floor
            outputBits.depend(inputs(0).output)
          }
          case "dceil" => {
            outputBits.double = inputs(0).output.double.ceil
            outputBits.depend(inputs(0).output)
          }
          case "dround" => {
            outputBits.double = inputs(0).output.double.round
            outputBits.depend(inputs(0).output)
          }
          case _ => ChiselError.error("Unsupported simulation operator in simulation: unary " + node.op)
        }
      case 2 =>
        node.op match {
          case "<<" => {
            outputBits.bigInt = inputs(0).output.bigInt << inputs(1).output.int
            for (i <- 0 to outputBits.highest - inputs(1).output.int) {
              outputBits(i + inputs(1).output.int) := inputs(0).output(i)
            }
            outputBits.depend(inputs(1).output)
          }
          case ">>" => {
            outputBits.bigInt = inputs(0).output.bigInt >> inputs(1).output.int
            for (i <- 0 to outputBits.highest - inputs(1).output.int) {
              outputBits(i) := inputs(0).output(i + inputs(1).output.int)
            }
            outputBits.depend(inputs(1).output)
          }
          case "s>>" => {
            outputBits.bigInt = inputs(0).output.sBigInt >> inputs(1).output.int
            for (i <- 0 to outputBits.highest - inputs(1).output.int) {
              outputBits(i) := inputs(0).output(i + inputs(1).output.int)
            }
            outputBits.depend(inputs(1).output)
          }
          case "+" => {
            outputBits.bigInt = inputs(0).output.bigInt + inputs(1).output.bigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "*" => {
            outputBits.bigInt = inputs(0).output.bigInt * inputs(1).output.bigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "s*s" => {
            outputBits.bigInt = inputs(0).output.sBigInt * inputs(1).output.sBigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "s*u" => {
            outputBits.bigInt = inputs(0).output.sBigInt * inputs(1).output.sBigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "/" => {
            if (inputs(1).output.bigInt != 0) {
              outputBits.bigInt = inputs(0).output.bigInt / inputs(1).output.bigInt
            } else {
              outputBits.bigInt = 0
            }
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "s/s" => {
            if (inputs(1).output.bigInt != 0) {
              outputBits.bigInt = inputs(0).output.sBigInt / inputs(1).output.sBigInt
            } else {
              outputBits.bigInt = 0
            }
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "%" => {
            if (inputs(1).output.bigInt != 0) {
              outputBits.bigInt = inputs(0).output.bigInt % inputs(1).output.bigInt
            } else {
              outputBits.bigInt = inputs(0).output.bigInt
            }
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "s%s" => {
            if (inputs(1).output.bigInt != 0) {
              outputBits.bigInt = inputs(0).output.sBigInt % inputs(1).output.sBigInt
            } else {
              outputBits.bigInt = inputs(0).output.sBigInt
            }
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "^" => {
            for (i <- 0 to outputBits.highest) {
              outputBits(i) := inputs(0).output(i).value != inputs(1).output(i).value
              outputBits(i).depend(inputs(0).output(i))
              outputBits(i).depend(inputs(1).output(i))
            }
          }
          case "-" => {
            outputBits.bigInt = inputs(0).output.bigInt - inputs(1).output.bigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "##" => {
            for (i <- 0 to inputs(0).output.highest) {
              outputBits(i + inputs(1).width) := inputs(0).output(i)
            }
            for (i <- 0 to inputs(1).output.highest) {
              outputBits(i) := inputs(1).output(i)
            }
          }
          case "&" => {
            for (i <- 0 to outputBits.highest) {
              outputBits(i) := inputs(0).output(i) && inputs(1).output(i)
              if (inputs(0).output(i) && inputs(1).output(i)) {
                outputBits(i).depend(inputs(0).output(i))
                outputBits(i).depend(inputs(1).output(i))
              } else {
                if (node.dependenceBias.size > 0 && !inputs(0).output(i) && !inputs(1).output(i)) {
                  for (in <- node.dependenceBias) {
                    outputBits(i).depend(inputs(in).output(i))
                  }
                } else {
                  for (in <- 0 until 2) {
                    if (!inputs(in).output(i)) {
                      outputBits(i).depend(inputs(in).output(i))
                    }
                  }
                }
              }
            }
          }
          case "|" => {
            for (i <- 0 to outputBits.highest) {
              outputBits(i) := inputs(0).output(i) || inputs(1).output(i)
              if (!(inputs(0).output(i) || inputs(1).output(i))) {
                outputBits(i).depend(inputs(0).output(i))
                outputBits(i).depend(inputs(1).output(i))
              } else {
                if (node.dependenceBias.size > 0 && inputs(0).output(i) && inputs(1).output(i)) {
                  for (in <- node.dependenceBias) {
                    outputBits(i).depend(inputs(in).output(i))
                  }
                } else {
                  for (in <- 0 until 2) {
                    if (inputs(in).output(i)) {
                      outputBits(i).depend(inputs(in).output(i))
                    }
                  }
                }
              }
            }
          }
          case "f+" => {
            outputBits.float = inputs(0).output.float + inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "f-" => {
            outputBits.float = inputs(0).output.float - inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "f*" => {
            outputBits.float = inputs(0).output.float * inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "f/" => {
            outputBits.float = inputs(0).output.float / inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "f%" => {
            outputBits.float = inputs(0).output.float % inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "fpow" => {
            outputBits.float = math.pow(inputs(0).output.float.toDouble, inputs(1).output.float.toDouble).toFloat
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d+" => {
            outputBits.double = inputs(0).output.double + inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d-" => {
            outputBits.double = inputs(0).output.double - inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d*" => {
            outputBits.double = inputs(0).output.double * inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d/" => {
            outputBits.double = inputs(0).output.double / inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d%" => {
            outputBits.double = inputs(0).output.double % inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "dpow" => {
            outputBits.double = math.pow(inputs(0).output.double, inputs(1).output.double)
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "==" => {
            outputBits(0) = inputs(0).output.bigInt == inputs(1).output.bigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "!=" => {
            outputBits(0) = inputs(0).output.bigInt != inputs(1).output.bigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "<" => {
            outputBits(0) = inputs(0).output.bigInt < inputs(1).output.bigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "<=" => {
            outputBits(0) = inputs(0).output.bigInt <= inputs(1).output.bigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "s<" => {
            outputBits(0) = inputs(0).output.sBigInt < inputs(1).output.sBigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "s<=" => {
            outputBits(0) = inputs(0).output.sBigInt <= inputs(1).output.sBigInt
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "f==" => {
            outputBits(0) = inputs(0).output.float == inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "f!=" => {
            outputBits(0) = inputs(0).output.float != inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "f>" => {
            outputBits(0) = inputs(0).output.float > inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "f<" => {
            outputBits(0) = inputs(0).output.float < inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "f<=" => {
            outputBits(0) = inputs(0).output.float <= inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "f>=" => {
            outputBits(0) = inputs(0).output.float >= inputs(1).output.float
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d==" => {
            outputBits(0) = inputs(0).output.double == inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d!=" => {
            outputBits(0) = inputs(0).output.double != inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d>" => {
            outputBits(0) = inputs(0).output.double > inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d<" => {
            outputBits(0) = inputs(0).output.double < inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d<=" => {
            outputBits(0) = inputs(0).output.double <= inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case "d>=" => {
            outputBits(0) = inputs(0).output.double >= inputs(1).output.double
            outputBits.depend(inputs(0).output)
            outputBits.depend(inputs(1).output)
          }
          case _ => ChiselError.error("Unsupported simulation operator in simulation: binary " + node.op + " @" + node.line.getFileName() + ":" + node.line.getLineNumber())
        }
      // case 3 =>
      //   node.op match {
      //     case "Mux" => {
      //       // outputBits := (if (inputs(0).output(0)) inputs(1).output else inputs(2).output)
      //       if (inputs(0).output(0)) {
      //         outputBits := inputs(1).output
      //         if (!node.dependenceBias.contains(2)) {
      //           outputBits.depend(inputs(0).output)
      //         }
      //         // for (i <- 0 to outputBits.highest) {
      //         //   outputBits(i).depend(inputs(0).output(0))
      //         //   outputBits(i).depend(inputs(1).output(i))
      //         // }
      //       } else {
      //         outputBits := inputs(2).output
      //         if (!node.dependenceBias.contains(1)) {
      //           outputBits.depend(inputs(0).output)
      //         }
      //         // for (i <- 0 to outputBits.highest) {
      //         //   outputBits(i).depend(inputs(0).output(0))
      //         //   outputBits(i).depend(inputs(2).output(i))
      //         // }
      //       }
      //     }
      //     case _ => ChiselError.error("Unsupported simulation operator in simulation: ternary " + node.op + " @" + node.line.getFileName() + ":" + node.line.getLineNumber())
      //   }
      case _ => ChiselError.error("Unsupported simulation operator in simulation: unsupported arity " + node.op + " @" + node.line.getFileName() + ":" + node.line.getLineNumber())
    }
    // System.err.println("Op " + node.line.getFileName() + ":" + node.line.getLineNumber() + " to " + output.toString())
  }

  override def staticDependencies(bit: SimulationBit): Set[SimulationBit] = {
    inputs.length match {
      case 1 =>
        node.op match {
          case "~" => return staticDependencyBit(inputs(0), bit)
          case "f-" => return staticDependencyBit(inputs(0), bit)
          case "d-" => return staticDependencyBit(inputs(0), bit)
          case _ => return super.staticDependencies(bit)
        }
      case 2 =>
        node.op match {
          case "##" => {
            for (i <- 0 to inputs(0).output.highest) {
              if (outputBits(i + inputs(1).width) == bit) {
                return Set(inputs(0).output(i))
              }
            }
            for (i <- 0 to inputs(1).output.highest) {
              if (outputBits(i) == bit) {
                return Set(inputs(1).output(i))
              }
            }
          }
          case "&" => return staticDependencyBit(inputs(0), bit) ++ staticDependencyBit(inputs(1), bit)
          case "|" => return staticDependencyBit(inputs(0), bit) ++ staticDependencyBit(inputs(1), bit)
          case _ => return super.staticDependencies(bit)
        }
      case 3 =>
        node.op match {
          case _ => return super.staticDependencies(bit)
        }
      case _ => return super.staticDependencies(bit)
    }
    return super.staticDependencies(bit)
  }

  override def staticDependents(bit: SimulationBit): Set[SimulationBit] = {
    inputs.length match {
      case 1 =>
        node.op match {
          case "~" => return staticDependentBit(inputs(0), bit)
          case "f-" => return staticDependentBit(inputs(0), bit)
          case "d-" => return staticDependentBit(inputs(0), bit)
          case _ => return super.staticDependents(bit)
        }
      case 2 =>
        node.op match {
          case "##" => {
            for (i <- 0 to inputs(0).output.highest) {
              if (inputs(0).output(i) == bit) {
                return Set(outputBits(i + inputs(1).width))
              }
            }
            for (i <- 0 to inputs(1).output.highest) {
              if (inputs(1).output(i) == bit) {
                return Set(outputBits(i))
              }
            }
          }
          case "&" => return staticDependentBit(inputs(0), bit) ++ staticDependentBit(inputs(1), bit)
          case "|" => return staticDependentBit(inputs(0), bit) ++ staticDependentBit(inputs(1), bit)
          case _ => return super.staticDependents(bit)
        }
      case 3 =>
        node.op match {
          case _ => return super.staticDependents(bit)
        }
      case _ => return super.staticDependents(bit)
    }
    return super.staticDependents(bit)
  }
}
