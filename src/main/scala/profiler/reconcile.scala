package profiler

import scala.collection.mutable.{Map, Queue, Set}


abstract class Difference(val left: Image, val right: Image) 


object Difference {
  
  def apply(left: Image, right: Image): Difference = {
    if (left == right)
      throw new NoDifference(left, right)
    if (null == right)
      return new Missing(left)
    if (null == left)
      return new Extra(right)
    // TODO Is this sensible?
    if (left.equals(right))
      throw new NoDifference(left, right)
    if (!left.declaredType.equals(right.declaredType))
      return TypeDifference(left, right)
    return FieldDifference(left, right)
  }
}


class Missing(left: Image) extends Difference(left, null) 


class Extra(right: Image) extends Difference(null, right) 


class FieldDifference(
    left: Image, 
    right: Image, 
    val different: Map[String, Pair[Image, Image]],
    val missing: Set[String], 	// Present only on the left side 
    val extra: Set[String]) 	// Present only on the right side
    extends Difference(left, right) {
}


  
object FieldDifference {
  
  def apply(left: Image, right: Image): FieldDifference = {
    val (different, missing, extra) = compare(left, right)
    return new FieldDifference(left, right, different, missing, extra)
  }
    
  def compare(left: Image, right: Image) = {
    // Naive is fine for now
    val same = Set[String]()
    val different = Map[String, Pair[Image, Image]]()
    val missing = Set[String]()
    val extra = Set[String]()
    left.fields.keys.foreach[Unit](name => { 
      if (!right.fields.contains(name))
        missing += name
      else {
        val lval = left(name)
        val rval = right(name)
        if (lval.equals(rval))
          same += name
        else
          different += ((name, (lval, rval)))
      }
    })
    right.fields.keys.foreach[Unit](name => {
      if (!left.contains(name))
        extra += name
    })
    (different, missing, extra)
  }
}


class TypeDifference(
    left: Image, 
    right: Image,
    different: Map[String, Pair[Image, Image]],
    missing: Set[String], 
    extra: Set[String]) 
    extends FieldDifference(left, right, different, missing, extra)


object TypeDifference {
  
  def apply(left: Image, right: Image): TypeDifference = {
    val (different, missing, extra) = FieldDifference.compare(left, right)
    return new TypeDifference(left, right, different, missing, extra)
  }
}


class NoDifference(left: Image, right: Image) extends Exception {}


class Reconciliation(
    val left: Profile, 
    val right: Profile) 
    extends Iterable[Difference] {
  
  val hits = Queue[Image]()
  
  val misses = Queue[Difference]()
  
  def +=(left: Image, right: Image): Unit = 
    try { +=(Difference(left, right)) }
    catch { 
      case x: NoDifference => hits += left
    }
  
  protected def +=(miss: Difference) = misses += miss 
    
  def iterator() = misses.iterator
}


object Reconciliation {

  def apply(left: Profile, right: Profile): Reconciliation = {
    val comparison = new Reconciliation(left, right)
    for (pair <- left.list().zip(right.list()))
      comparison += (pair._1, pair._2)
    return comparison
  }
}

