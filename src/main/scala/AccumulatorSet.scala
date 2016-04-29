/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.{Set,TreeSet,HashSet}

object AccumulatorSet {
  var count: Long = 0
  var validCount = 0
  var origins = Set[AccumulatorSet[_]]()

  def flattenAll() {
    while (origins.size > 0) {
      // System.err.println("Flattening... " + origins.size + "/" + validCount)
      val oldOrigins = origins
      origins = Set()
      for (origin <- oldOrigins) {
        origin.flatten_up()
      }
    }
  }

  implicit def toSet[T](accSet: AccumulatorSet[T]): Set[T] = accSet.immediates
}


/**
 * The purpose of this set data structure is to allow fast
 * bulk-additions (fast creation of supersets). Sacrifices lookup.
 *
 * Sets should not be added cyclically.
 */
class AccumulatorSet[T] extends Ordered[AccumulatorSet[T]] {
  val id = AccumulatorSet.count
  AccumulatorSet.count += 1
  var immediates = Set[T]()
  var supersets = Set[AccumulatorSet[T]]()
  var subsets = Set[AccumulatorSet[T]]()
  var frozen = false
  var preserved = false
  var valid = true
  AccumulatorSet.validCount += 1

  def ++= (subset: AccumulatorSet[T]): Unit = {
    this.subsets += subset
    subset.supersets += this
  }

  def += (item: T): Unit = {
    immediates += item
  }

  def freeze(): Unit = {
    frozen = true
    if (subsets.size == 0) {
      AccumulatorSet.origins += this
    }
  }

  def unfreeze(): Unit = {
    frozen = false
    if (subsets.size != 0) {
      AccumulatorSet.origins -= this
    }
  }

  /* Used to protect from destruction by flattening */
  def preserve(): Unit = {
    freeze()
    preserved = true
  }

  def unpreserve(): Unit = {
    preserved = false
    collapse()
  }

  def merge[U](a: Set[U], b: Set[U]): Set[U] = {
    // a ++ b
    if (a.size > b.size) {
      a ++= b
    } else {
      b ++= a
    }
  }
  // def merge[U](a: TreeSet[U], b: TreeSet[U]): TreeSet[U] = {
  //   // a ++ b
  //   if (a.size > b.size) {
  //     a ++= b
  //   } else {
  //     b ++= a
  //   }
  // }
  // def merge[U](a: HashSet[U], b: HashSet[U]): HashSet[U] = {
  //   // a ++ b
  //   if (a.size > b.size) {
  //     a ++= b
  //   } else {
  //     b ++= a
  //   }
  // }

  /** Perfrom graph simplification */
  def collapse(): Unit = {
    if (!frozen || !valid) {
      return
    }
    if (subsets.size == 0) {
      AccumulatorSet.origins += this
    }
    if (supersets.size == 0) {
      AccumulatorSet.validCount -= 1
      valid = false
      for (subset <- subsets) {
        subset.supersets -= this
      }
      AccumulatorSet.origins -= this
      subsets.foreach(_.collapse())
    } else if (supersets.size == 1) {
      AccumulatorSet.validCount -= 1
      valid = false
      val superset = supersets.head
      superset.immediates = merge(immediates, superset.immediates)
      for (subset <- subsets) {
        subset.supersets -= this
        subset.supersets += superset
      }
      superset.subsets = merge(subsets, superset.subsets)
      superset.subsets -= this
      // AccumulatorSet.origins -= this
      subsets.foreach(_.collapse())
    }
  }

  /**
   * Feed all immediates up to the supersets.
   *
   * This needs to be done bottom up!
   * After running, this becomes invalid.
   */
  def flatten_up(): Unit = {
    var itNo = 0
    // TODO: Optimise so that last superset is a merge operation
    for (superset <- supersets) {
      itNo += 1
      if (!preserved && itNo == supersets.size) {
        AccumulatorSet.validCount -= 1
        valid = false
        superset.immediates = merge(superset.immediates, immediates)
      } else {
        superset.immediates ++= immediates
      }
      superset.subsets -= this
      if (superset.supersets.size > 0 && superset.subsets.size == 0) {
        AccumulatorSet.origins += superset
      }
    }
    supersets.clear()
  }

  override def finalize(): Unit = {
    if (valid) {
      AccumulatorSet.validCount -= 1
    }
  }

  /**
   * Test membership
   *
   * Flattening must be done first
   */
  def contains(item: T): Boolean = {
    immediates.contains(item)
  }

  def compare(that: AccumulatorSet[T]): Int = {
    if (this.id < that.id) {
      -1
    } else if (this.id == that.id) {
      0
    } else {
      1
    }
  }
}
