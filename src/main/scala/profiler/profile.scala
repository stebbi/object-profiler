package profiler  

import java.lang.reflect.Field
import java.lang.reflect.Method
import org.w3c.dom.Document
import scala.collection.mutable.{Map, Queue, Set}
import scala.xml.Elem


/**
 * An image is a snapshot of an object at a given moment.
 */
class Image(
    /** The subject of the snapshot. */
    val subject: AnyRef,
    /** The declared type of the subject. */
    val declaredType: Class[_], 
    /** The object tree depth at which the subject was first encountered. */
    val depth: Int) {
  
  protected val fields = Map[String, (Image, Boolean)]()
  
  def bind(name: String, value: Image, isProperty: Boolean = false): Unit = 
    fields += ((name, (value, isProperty)))
   
  /** 
   * Accessor for field and property values. 
   * @param name The name of the field or property to access.
   */
  def apply(name: String): (Image, Boolean) = fields(name)
  
  def contains(name: String): Boolean = fields.contains(name) 
  
  def equals(other: Image): Boolean = 
    if (null != subject) subject.equals(other.subject) else false
  
  def hash(): Int = profiler.hash(subject)
    
  def isInScope(): Boolean = {
    subject match { 
      case null => false
      case _: java.lang.Boolean => false
      case _: java.lang.Byte => false
      case _: java.lang.Character => false
      case _: java.lang.Class[_] => false
      case _: java.lang.Double => false
      case _: java.lang.Integer => false
      case _: java.lang.Long => false
      case _: java.lang.Short => false
      case _: java.lang.String => false
      case _ => true
    }
  }

  def actualType(): Class[_] = 
    if (null != subject) subject.getClass else classOf[Object] 

  def visit(visitor: (String, Image, Boolean) => AnyRef): Iterable[AnyRef] = 
    for ((name, (value, isProperty)) <- fields)
      yield visitor(name, value, isProperty)
}


class Schedule(root: Image) {
  
  val remaining = Map((root.hash, root)) 

  val schedule = Queue(root)

  def +=(image: Image): Unit = {
    if (!image.isInScope()) 
      return
    if (contains(image)) 
      return
    remaining += ((image.hash, image))
    schedule.enqueue(image)
  }

  def contains(image: Image): Boolean = contains(image.hash)
    
  def contains(key: Int): Boolean = remaining.contains(key)
    
  def isEmpty(): Boolean = remaining.isEmpty
    
  def next(): Image = {
    val next = schedule.dequeue
    remaining.remove(next.hash)
    next
  }
  
  def apply(key: Int): Image = remaining(key)
} 


class Profile extends Iterable[Image] {
  
  private val images = Map[Int, Image]()
  
  private val order = Queue[Image]()
  
  def root(): Image = order.head
  
  def +=(image: Image): Unit = {
    if (images.contains(image.hash)) 
      return 
    images += ((image.hash, image))
    order += image
  }
    
  def contains(key: Int): Boolean = images.contains(key)
    
  def apply(key: Int): Image = images(key)
  
  def list() = order.result
  
  def iterator() = order.result.iterator
}


class DuplicateImage(duplicate: Image, original: Image) extends Exception


class Profiler(
    val root: AnyRef, 
    val limit: Int = 1) {

  private val recorded = new Profile()
  
  private val scheduled = new Schedule(new Image(root, root.getClass(), 0))

  def profile(): Profile = visit()._1
  
  protected def visit(): (Profile, Schedule) = {
    if (scheduled.isEmpty)
      return (recorded, scheduled)
    var current: Image = null
    do { current = scheduled.next() } 
    while (recorded.contains(hash(current)))
    if (current.depth >= limit)
      return (recorded, scheduled)
    visit(current).foreach[Unit](image => 
      if (null != image && !recorded.contains(hash(image))) 
        scheduled += image)
    recorded += current
    visit() 
  }

  protected def visit(current: Image): Queue[Image] = {
    val later = Queue[Image]()
    val c = current.subject.getClass()
    for (field <- c.getFields()) 
      later += visit(current, field)
    for (field <- c.getDeclaredFields()) 
      later += visit(current, field)
    for (method <- c.getMethods()) 
      later += visit(current, method)
    later
  }
  
  protected def visit(current: Image, field: Field): Image = {
    field.setAccessible(true)
    val name = field.getName()
    val value = field.get(current.subject)
    val declaredType = field.getType()
    visit(current, name, value, declaredType)
  }

  protected def visit(current: Image, method: Method): Image = {
    if (0 != method.getParameterTypes().length)
      return null
    val name = method.getName()
    if (!name.startsWith("get"))
      return null
    method.setAccessible(true)
    val value = method.invoke(current.subject)
    val declaredType = method.getReturnType()
    visit(current, name, value, declaredType, true)
  }

  protected def visit(
    current: Image, 
    name: String, value: Object, declaredType: Class[_], 
    isProperty: Boolean = false)
  : Image = {
    val hash = profiler.hash(value)
    val isRecorded = recorded.contains(hash)
    val isScheduled = scheduled.contains(hash)
    val later = if (isRecorded) recorded(hash)
      else if (isScheduled) scheduled(hash)
      else new Image(value, declaredType, current.depth + 1)
    current.bind(name, later, isProperty)
    if (!isRecorded && !isScheduled)
      later
    else null
  }
}

