package Chisel {

import Component._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.BufferProxy;
import scala.math._;
import Vec._

object log2up
{
  def apply(in: Int) = if (in == 1) 1 else ceil(log(in)/log(2)).toInt
}

object UFixToOH
{
  def apply(in: UFix, width: Int): Bits =
  {
    if(chiselOneHotMap.contains((in, width)))
      chiselOneHotMap((in, width))
    else {
      val out = Bits(1, width)
      val res = (out << in)(width-1,0)
      chiselOneHotMap += ((in, width) -> res)
      res
    }
  }
}

class Mux1H_(n: Int, w: Int) extends Component
{
  val io = new Bundle {
    val sel = Vec(n) { Bool(dir = INPUT) }
    val in = Vec(n) { Bits(width = w, dir = INPUT) }
    val out = Bits(width = w, dir = OUTPUT)
  }

  if (n > 1) {
    var out = io.in(0) & Fill(w, io.sel(0))
    for (i <- 1 to n-1)
      out = out | (io.in(i) & Fill(w, io.sel(i)))
    io.out := out
  } else {
    io.out := io.in(0)
  }
}

object MuxN {
  def apply(sel: Seq[Bits], in: Seq[Bits]): Bits = {
    if (in.size == 0)
      null
    else if (in.size == 1)
      in(0)
    else
      Mux(sel(log2up(in.size)-1), apply(sel, in.slice((1 << log2up(in.size))/2, in.size)), apply(sel, in.slice(0, (1 << log2up(in.size))/2)))
  }
  def apply(sel: Bits, in: Seq[Bits]): Bits = apply((0 until log2up(in.size)).map(sel(_)), in)
}

object Mux1H {
  def apply(w: Int = -1, pairs: Seq[(Bool, Bits)]): Bits = {
    val inferredWidth = (w +: pairs.map{case(bool, bits) => bits.getWidth}).max
    assert(inferredWidth > 0, {println("UNABLE TO INFER WIDTH ON MUX1H")})
    
    pairs.map{case(bool, bits) => bits & Fill(inferredWidth, bool)}.reduceLeft(_ | _)
  }

  
  def apply[T <: Data](pairs: Seq[(Bool, T)], gen: () => T): T = {
    val res = gen().asOutput
    res.setIsTypeNode
    
    val inferredWidth = (pairs.map{case(bool, data) => data.getWidth}).max
    assert(inferredWidth > 0, {println("UNABLE TO INFER WIDTH ON MUX1H")})

    var filter: List[List[Bits]] = pairs(0)._2.flatten.map(x => List()).toList
    for((bool, data) <- pairs) {
      filter = (filter zip data.flatten).map{case(a, (b, c)) => a :+ c}
    }

    val bools = pairs.map{case(bool, data) => bool}

    for(((n, i), data) <- res.flatten zip filter) {
      val p = new ArrayBuffer[(Bool, Bits)]
      for((b, d) <- bools zip data)
        p += (b -> d.toBits)
      i.inputs(0) = Mux1H(-1, p)
    }

    res
  }
}

object VecBuf{
  def apply[T <: Data](n: Int)(gen: => Vec[T]): ArrayBuffer[Vec[T]] = {
    val res = new ArrayBuffer[Vec[T]]
    for(i <- 0 until n)
      res += gen
    res
  }
}

object Vec {
  def apply[T <: Data](n: Int)(gen: => T): Vec[T] = {
    val res = new Vec[T](() => gen);
    for(i <- 0 until n){
      val t   = gen;
      res    += t;
      t.name += i;
    }
    res.eltWidth = res(0).getWidth
    res
  }
  
  def getEnable(onehot: Bits, i: Int): Bool = {
    var enable: Bool = null
      if(chiselOneHotBitMap.contains(onehot, i)){
        enable = chiselOneHotBitMap(onehot, i)
      } else {
        enable = onehot(i).toBool
        chiselOneHotBitMap += ((onehot, i) -> enable)
      }
    enable
  }
}

class VecProc extends proc {
  var addr: UFix = null
  var elms: ArrayBuffer[Bits] = null

  override def genMuxes(default: Node) = {}

  def procAssign(src: Node) = {
    val onehot = UFixToOH(addr, elms.length)
    searchAndMap = true
    for(i <- 0 until elms.length){
      when (getEnable(onehot, i)) {
        if(elms(i).comp != null)
          elms(i).comp procAssign src
        else
          elms(i) procAssign src
      }
    }
    searchAndMap = false
  }
}

class Vec[T <: Data](val gen: () => T) extends Data with Cloneable with BufferProxy[T] { 
  var eltWidth = 0
  val self = new ArrayBuffer[T]
  val readPortCache = new HashMap[UFix, T]
  var sortedElementsCache: ArrayBuffer[ArrayBuffer[Bits]] = null
  var flattenedVec: Node = null
  override def apply(idx: Int): T = {
    super.apply(idx)
  };

  def sortedElements: ArrayBuffer[ArrayBuffer[Bits]] = {
    if (sortedElementsCache == null) {
      sortedElementsCache = new ArrayBuffer[ArrayBuffer[Bits]]
      
      // create buckets for each elm in data type
      for(i <- 0 until this(0).flatten.length) 
        sortedElementsCache += new ArrayBuffer[Bits]

      // fill out buckets
      for(elm <- this) {
        for(((n, io), i) <- elm.flatten zip elm.flatten.indices) {
          //val bits = io.toBits
          //bits.comp = io.comp
          sortedElementsCache(i) += io.asInstanceOf[Bits]
        }
      }
    }
    sortedElementsCache
  }
  
  def apply(ind: UFix): T = 
    read(ind)

  def apply(ind: Bits): T =
    read(ind)

  def write(addr: UFix, data: T) = {
    if(data.isInstanceOf[Node]){

      val onehot = UFixToOH(addr, length)
      searchAndMap = true
      for(i <- 0 until length){
        when (getEnable(onehot, i)) {
          this(i).comp procAssign data.toNode
        }
      }
      searchAndMap = false
    }
  }

  def write(addr: Bits, data: T): Unit = {
    write(addr.toUFix, data)
  }

  def read(addr: UFix): T = {
    if(readPortCache.contains(addr))
      return readPortCache(addr)

    val res = this(0).clone
    //val res = gen()
    res.setIsTypeNode
    for(((n, io), sortedElm) <- res.flatten zip sortedElements) {
      val w = io.getWidth
      val onehot = UFixToOH(addr, length)
      val pairs = new ArrayBuffer[(Bool, Bits)]
      for(i <- 0 until length){
        pairs += (getEnable(onehot, i) -> sortedElm(i))
      }
      val io_res = Mux1H(w, pairs)
      io assign io_res

      // setup the comp for writes
        val io_comp = new VecProc()
        io_comp.addr = addr
        io_comp.elms = sortedElm
        io.comp = io_comp
    }
    readPortCache += (addr -> res)
    return res
  }

  override def flatten: Array[(String, Bits)] = {
    val res = new ArrayBuffer[(String, Bits)];
    for (elm <- self)
      elm match {
	case bundle: Bundle => res ++= bundle.flatten;
	case io: Bits         => res += ((io.name, io));
      }
    res.toArray
  }

  override def <>(src: Node) = {
    src match {
      case other: Vec[T] => {
	for((b, o) <- self zip other.self)
	  b <> o
      }
    }
  }

  override def ^^(src: Node) = {
    src match {
      case other: Vec[T] => 
	for((b, o) <- self zip other.self)
	  b ^^ o
    }
  }

  def <>(src: Vec[T]) = {
    for((b, e) <- self zip src)
      b <> e;
  }

  def <>(src: Iterable[T]) = {
    for((b, e) <- self zip src)
      b <> e;
  }

  // TODO: CHECK FOR ALL OUT
  def :=[T <: Data](src: Vec[T]) = {
    for((me, other) <- this zip src){
      if(other.isInstanceOf[Bundle])
        me.asInstanceOf[Bundle] := other.asInstanceOf[Bundle]
      else
        me := other
    }
  }

  def := (src: Bits) = {
    for(i <- 0 until length)
      this(i) := src(i)
  }

  override def traceableNodes = self.toArray

  override def removeTypeNodes() = {
    for(bundle <- self)
      bundle.removeTypeNodes
  }

  override def flip(): this.type = {
    for(b <- self)
      b.flip();
    this
  }

  override def name_it (path: String, named: Boolean = true) = {
    if(!this.named) {
      if(path.length > 0) name = path
      this.named = named
      for (i <- self) {
        i.name_it( (if (path.length > 0) path + "_" else "") + i.name, named )
      }
    }
  }

  override def clone(): this.type = {
    val res = Vec(size){ gen() };
    res.asInstanceOf[this.type]
  }

  override def toNode: Node = {
    if(flattenedVec == null){
      val nodes = flatten.map{case(n, i) => i};
      flattenedVec = Concatenate(nodes.head, nodes.tail.toList: _*)
    }
    flattenedVec
  }

  override def fromNode(n: Node): this.type = {
    val res = this.clone();
    var ind = 0;
    for((name, io) <- res.flatten.toList.reverse) {
      io.asOutput();
      if(io.width > 1)
	io assign NodeExtract(n, ind + io.width-1, ind)
      else
	io assign NodeExtract(n, ind);
      ind += io.width;
    }
    res
  }

  override def asOutput(): this.type = {
    for(elm <- self)
      elm.asOutput;
    this
  }

  override def asInput(): this.type = {
    for(elm <- self)
      elm.asInput
    this
  }

  override def setIsTypeNode() = {
    isTypeNode = true;
    for(elm <- self)
      elm.setIsTypeNode
  }

  override def toBits(): Bits = {
    // var res: Bits = null
    // for(i <- 0 until length)
    //   res = Cat(this(i), res)
    // res
    val reversed = this.reverse
    Cat(reversed.head, reversed.tail: _*)
  }
}

}
