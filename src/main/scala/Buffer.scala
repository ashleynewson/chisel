/*
  Copyright Ashley Newson 2016
*/

package Chisel

/** Constructs a node which can be used to aid slice analysis. */
object Buffer {
  def apply[T <: Node](n: T): T = {
    // Being lazy for now
    // Mux(Bool(true), n, n)
    n match {
      case d: Data => {
        val buf = d match {
          case _: Bool => Bool(OUTPUT)
          case _: UInt => UInt(OUTPUT)
          case _: SInt => SInt(OUTPUT)
          // case _ => {return Mux(Bool(true), d, d).asInstanceOf[T]}
        }
        // return Mux(Bool(true), d, d).asInstanceOf[T];
        // buf.inputs += n // Don't do :=, as that might depend on this function
        return buf.init("", Node.widthOf(0), n).asInstanceOf[T]
      }
      case _ => return n
    }
  }
}
