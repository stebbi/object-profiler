
/** 
 * Entry points are provided for profiling objects, reconciling profiles, 
 * serializing profiles and reconciliations to XML, and deserializing 
 * profiles from XML.
 */
package object profiler {

  import java.io.{File, FileOutputStream, OutputStream}
  import scala.collection.mutable.Map
  import scala.xml.{Elem, Node, Text}
  import scala.xml.parsing.ConstructingParser

  /**
   * Produces a profile of the subject parameter, down to the parameter depth.
   *
   * If the subject is <code>new java.io.File("/tmp/test.txt")</code>, and the 
   * depth is 1, then the profile will contain an image of the file object, 
   * with a map of the fields and properties of that object. 
   * 
   * @param subject The object to produce a profile of.
   * @param depth The depth the profile should reach.
   */
  def profile(subject: AnyRef, depth: Int): Profile = 
    new Profiler(subject, depth).profile()

  def reconcile(left: AnyRef, right: AnyRef, depth: Int): Reconciliation = 
    new Reconciliation(profile(left, depth), profile(right, depth), depth)

  def prettify(xml: scala.xml.Elem): String = 
    new scala.xml.PrettyPrinter(80, 2).format(xml)

  def report(subject: AnyRef, depth: Int): Unit = 
    report(subject, depth, System.out)

  def report(subject: AnyRef, depth: Int, path: String): Unit = 
    serialize(profile(subject, depth), depth, path)

  def report(subject: AnyRef, depth: Int, out: OutputStream): Unit = 
    serialize(profile(subject, depth), depth, out)

  def report(left: AnyRef, right: AnyRef, depth: Int): Unit = 
    report(left, right, depth, System.out)

  def report(left: AnyRef, right: AnyRef, depth: Int, path: String): Unit = 
    serialize(
      reconcile(left, right, depth), 
      depth, 
      new FileOutputStream(path))

  def report(left: AnyRef, right: AnyRef, depth: Int, out: OutputStream)
  : Unit = 
    serialize(reconcile(left, right, depth), depth, out)

  def load(path: String): Profile = load(new File(path))

  def load(f: File): Profile = 
    new Profile() += deserializeObject(
      ConstructingParser.fromFile(f, false).document().docElem, 0)

  def deserializeObject(e: Node, depth: Int): Image = {
    val attribute = e.attributes.asAttrMap
    // val actualType = Class.forName(attribute("actualType"))
    val declaredType = Class.forName(attribute("declaredType"))
    val fields = Map[String, (Image, Boolean)]()
    for (c <- e.child) 
      if ("field".equals(c.label))
        fields += deserializeField(c, depth)
    new Image(null, declaredType, depth, fields)
  }

  def deserializeField(e: Node, depth: Int): (String, (Image, Boolean)) = {
    val attribute = e.attributes.asAttrMap
    val name = attribute("name")
    val declaredType = attribute("declaredType") match {
      case "boolean" => classOf[java.lang.Boolean]
      case "char" => classOf[java.lang.Character]
      case "double" => classOf[java.lang.Double]
      case "float" => classOf[java.lang.Float]
      case "int" => classOf[java.lang.Integer]
      case "long" => classOf[java.lang.Long]
      case n => Class.forName(n)
    }
    val subject = e.nonEmptyChildren.head match {
      case c: Text => instantiate(declaredType, c.toString)
      case c: Elem => deserializeObject(c, depth - 1)
      case c => throw new Exception("Unexpected: " + c.toString)
    }
    val image = new Image(subject, declaredType, depth)
    ((name, (image, false)))
  } 

  def instantiate(clss: Class[_], value: String): Object = 
    if (classOf[java.lang.String].equals(clss))
      value
    else if (classOf[java.lang.Boolean].equals(clss))
      java.lang.Boolean.valueOf(value)
    else if (classOf[java.lang.Character].equals(clss))
      java.lang.Character.valueOf(value.charAt(0))
    else if (classOf[java.lang.Double].equals(clss))
      java.lang.Double.valueOf(value)
    else if (classOf[java.lang.Integer].equals(clss))
      java.lang.Integer.valueOf(value)
    else if (classOf[java.lang.Long].equals(clss))
      java.lang.Long.valueOf(value)
    else
      throw new UnsupportedOperationException(String.format(
        "Unable to instantiate %s with %s", clss.getCanonicalName, value))

  // TODO Fix
  def describe(subject: Any): String = 
    subject match {
    case null => "null"
    case _ => subject.asInstanceOf[AnyRef].getClass().getCanonicalName() + "@" + subject.hashCode().toString()
    }
  
  def hash(subject: Any): Int = describe(subject).hashCode()
}

