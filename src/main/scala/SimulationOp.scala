/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationOp(node: Op) extends SimulationNode(node) {
  override val clocked = false

  override def evaluate(): Unit = {
    inputs.length match {
      case 1 =>
        node.op match {
          case "~" =>
            for (i <- 0 to outputBits.highest) {
              outputBits(i) = !inputs(0).output(i)
            }
          case "f-" => {
            outputBits := inputs(0).output
            outputBits(31) = !inputs(0).output(31)
          }
          case "fsin" => outputBits.float = math.sin(inputs(0).output.float.toDouble).toFloat
          case "fcos" => outputBits.float = math.cos(inputs(0).output.float.toDouble).toFloat
          case "ftan" => outputBits.float = math.tan(inputs(0).output.float.toDouble).toFloat
          case "fsqrt" => outputBits.float = math.sqrt(inputs(0).output.float.toDouble).toFloat
          case "flog" => outputBits.float = math.log(inputs(0).output.float.toDouble).toFloat
          case "ffloor" => outputBits.float = inputs(0).output.float.floor
          case "fceil" => outputBits.float = inputs(0).output.float.ceil
          case "fround" => outputBits.float = inputs(0).output.float.round
          case "d-" => {
            outputBits := inputs(0).output
            outputBits(63) = !inputs(0).output(63)
          }
          case "dsin" => outputBits.double = math.sin(inputs(0).output.double)
          case "dcos" => outputBits.double = math.cos(inputs(0).output.double)
          case "dtan" => outputBits.double = math.tan(inputs(0).output.double)
          case "dsqrt" => outputBits.double = math.sqrt(inputs(0).output.double)
          case "dlog" => outputBits.double = math.log(inputs(0).output.double)
          case "dfloor" => outputBits.double = inputs(0).output.double.floor
          case "dceil" => outputBits.double = inputs(0).output.double.ceil
          case "dround" => outputBits.double = inputs(0).output.double.round
          case _ => ChiselError.error("Unsupported simulation operator in simulation: unary " + node.op)
        }
      case 2 =>
        node.op match {
          case "<<" => outputBits.bigInt = inputs(0).output.bigInt << inputs(1).output.int
          case ">>" => outputBits.bigInt = inputs(0).output.bigInt >> inputs(1).output.int
          case "s>>" => outputBits.bigInt = inputs(0).output.sBigInt >> inputs(1).output.int
          case "+" => outputBits.bigInt = inputs(0).output.bigInt + inputs(1).output.bigInt
          case "*" => outputBits.bigInt = inputs(0).output.bigInt * inputs(1).output.bigInt
          case "s*s" => outputBits.bigInt = inputs(0).output.sBigInt * inputs(1).output.sBigInt
          case "s*u" => outputBits.bigInt = inputs(0).output.sBigInt * inputs(1).output.sBigInt
          case "/" => outputBits.bigInt = inputs(0).output.bigInt / inputs(1).output.bigInt
          case "s/s" => outputBits.bigInt = inputs(0).output.sBigInt / inputs(1).output.sBigInt
          case "%" => outputBits.bigInt = inputs(0).output.bigInt % inputs(1).output.bigInt
          case "s%s" => outputBits.bigInt = inputs(0).output.sBigInt % inputs(1).output.sBigInt
          case "^" =>
            for (i <- 0 to outputBits.highest) {
              outputBits(i) = inputs(0).output(i) != inputs(1).output(i)
            }
          case "-" => outputBits.bigInt = inputs(0).output.bigInt - inputs(1).output.bigInt
          case "##" => {
            for (i <- 0 to inputs(0).output.highest) {
              outputBits(i + inputs(1).width) = inputs(0).output(i)
            }
            for (i <- 0 to inputs(1).output.highest) {
              outputBits(i) = inputs(1).output(i)
            }
          }
          case "&" =>
            for (i <- 0 to outputBits.highest) {
              outputBits(i) = inputs(0).output(i) && inputs(1).output(i)
            }
          case "|" =>
            for (i <- 0 to outputBits.highest) {
              outputBits(i) = inputs(0).output(i) || inputs(1).output(i)
            }
          case "f+" => outputBits.float = inputs(0).output.float + inputs(1).output.float
          case "f-" => outputBits.float = inputs(0).output.float - inputs(1).output.float
          case "f*" => outputBits.float = inputs(0).output.float * inputs(1).output.float
          case "f/" => outputBits.float = inputs(0).output.float / inputs(1).output.float
          case "f%" => outputBits.float = inputs(0).output.float % inputs(1).output.float
          case "fpow" => outputBits.float = math.pow(inputs(0).output.float.toDouble, inputs(1).output.float.toDouble).toFloat
          case "d+" => outputBits.double = inputs(0).output.double + inputs(1).output.double
          case "d-" => outputBits.double = inputs(0).output.double - inputs(1).output.double
          case "d*" => outputBits.double = inputs(0).output.double * inputs(1).output.double
          case "d/" => outputBits.double = inputs(0).output.double / inputs(1).output.double
          case "d%" => outputBits.double = inputs(0).output.double % inputs(1).output.double
          case "dpow" => outputBits.double = math.pow(inputs(0).output.double, inputs(1).output.double)
          case "===" => outputBits(0) = inputs(0).output.bigInt == inputs(1).output.bigInt
          case "!=" => outputBits(0) = inputs(0).output.bigInt != inputs(1).output.bigInt
          case "<" => outputBits(0) = inputs(0).output.bigInt < inputs(1).output.bigInt
          case "<=" => outputBits(0) = inputs(0).output.bigInt <= inputs(1).output.bigInt
          case "s<" => outputBits(0) = inputs(0).output.sBigInt < inputs(1).output.sBigInt
          case "s<=" => outputBits(0) = inputs(0).output.sBigInt <= inputs(1).output.sBigInt
          case "f==" => outputBits(0) = inputs(0).output.float == inputs(1).output.float
          case "f!=" => outputBits(0) = inputs(0).output.float != inputs(1).output.float
          case "f>" => outputBits(0) = inputs(0).output.float > inputs(1).output.float
          case "f<" => outputBits(0) = inputs(0).output.float < inputs(1).output.float
          case "f<=" => outputBits(0) = inputs(0).output.float <= inputs(1).output.float
          case "f>=" => outputBits(0) = inputs(0).output.float >= inputs(1).output.float
          case "d==" => outputBits(0) = inputs(0).output.double == inputs(1).output.double
          case "d!=" => outputBits(0) = inputs(0).output.double != inputs(1).output.double
          case "d>" => outputBits(0) = inputs(0).output.double > inputs(1).output.double
          case "d<" => outputBits(0) = inputs(0).output.double < inputs(1).output.double
          case "d<=" => outputBits(0) = inputs(0).output.double <= inputs(1).output.double
          case "d>=" => outputBits(0) = inputs(0).output.double >= inputs(1).output.double
          case _ => ChiselError.error("Unsupported simulation operator in simulation: binary " + node.op)
        }
      case 3 =>
        node.op match {
          case "Mux" =>
            outputBits := (if (inputs(0).output(0)) inputs(1).output else inputs(2).output)
          case _ => ChiselError.error("Unsupported simulation operator in simulation: ternary " + node.op)
        }
      case _ => ChiselError.error("Unsupported simulation operator in simulation: unsupported arity " + node.op)
    }
    System.err.println("Op " + node.line.getFileName() + ":" + node.line.getLineNumber() + " to " + output.toString())
  }
}
