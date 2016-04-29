/*
  Copyright Ashley Newson 2016
*/

package Chisel

/** Constructs a node which can be used to aid slice analysis. */
object Buffer {
  def apply[T <: Node](n: T): T = {
    // Being lazy for now
    // Mux(Bool(true), n, n)
    if (Driver.refineNodeStructure) {
      return n
    }
    n.litOpt match {
      case Some(l) => {
        // Breaks if we buffer a literal.
        // Not sure why.
        n match {
          case d: Data => {
            val buf = d match {
              case _: Bool => Bool(l.value > 0)
              case _: UInt => UInt(l.value)
              case _: SInt => SInt(l.value)
            }
            buf.inputs(0).inputs += l // Hack hack hack
            return buf.asInstanceOf[T]
          }
          case _ => {
            return n
          }
        }
      }
      case None => {
        n match {
          case d: Data => {
            val buf = d match {
              case _: Bool => Bool(OUTPUT)
              case _: UInt => UInt(OUTPUT)
              case _: SInt => SInt(OUTPUT)
            }
            // buf.inputs += n // Don't do :=, as that might depend on this function
            return buf.init("", Node.widthOf(0), n).asInstanceOf[T]
          }
          case _ => {
            return n
          }
        }
      }
    }
  }
}
