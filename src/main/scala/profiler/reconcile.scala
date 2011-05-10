package profiler

import scala.collection.mutable.{Map, Queue, Set}


abstract class Difference(
    val name: String, 
    val left: Image, 
    val right: Image) 


object Difference {
  
  def apply(name: String, left: Image, right: Image, depth: Int): Difference = {
    if (left == right)
      throw new NoDifference(name, left, right)
    if (null == right)
      return new Missing(name, left)
    if (null == left)
      return new Extra(name, right)
    if (left.equals(right)) // TODO Fix
      throw new NoDifference(name, left, right)
    if (!left.declaredType.equals(right.declaredType))
      return new TypeDifference(name, left, right)
    return new Reconciliation(name, left, right, depth)
  }
}


class Missing(name: String, left: Image) extends Difference(name, left, null) 


class Extra(name: String, right: Image) extends Difference(name, null, right) 


class FieldDifference(
    name: String, 
    left: Image, 
    right: Image) 
    extends Difference(name, left, right) {}

  
class TypeDifference(
    name: String, 
    left: Image, 
    right: Image)
    extends FieldDifference(name, left, right) {}


class NoDifference(name: String, left: Image, right: Image) extends Exception {}


class Reconciliation(
    name: String, 
    left: Image, 
    right: Image, 
    depth: Int) 
    extends Difference(name, left, right) 
    with Iterable[Difference] {

  private val differences = Map[String, Difference]()

  compare(left, right, depth)

  def this(left: Profile, right: Profile, depth: Int) = 
    this("root", left.root, right.root, depth)

  def compare(left: Image, right: Image, depth: Int): Unit  = {
    if (1 > depth)
      return
    if (left.declaredType != right.declaredType) {
      differences += ((name, new TypeDifference(name, left, right)))
      return
    }
    val visited = Set[String]()
    left.visit((name: String, lval: Image, isProperty: Boolean) => {
      visited += name
      if (!right.contains(name)) 
        differences += ((name, new Missing(name, lval)))
      else {
        val rval = right(name)._1
        if (!lval.equals(rval))
          differences += { 
            if (!lval.isInScope() || !rval.isInScope())
              ((name, new FieldDifference(name, lval, rval)))
            else
              ((name, new Reconciliation(name, lval, rval, depth - 1))) 
          }
      }
      Unit
    })
    right.visit((name: String, rval: Image, isProperty: Boolean) => {
      if (!visited.contains(name))
        differences += ((name, new Extra(name, rval)))
      Unit
    }) 
  }

  def visit(visitor: Difference => AnyRef) = 
    for (difference <- differences.values)
      yield visitor(difference)

  def iterator() = differences.values.iterator
}


