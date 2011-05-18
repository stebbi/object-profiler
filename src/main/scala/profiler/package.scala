
/** 
 * Entry points are provided for profiling objects, reconciling profiles, 
 * serializing profiles and reconciliations to XML, and deserializing 
 * profiles from XML.
 */
package object profiler {

  import java.io.{File, FileOutputStream, OutputStream}
  import scala.collection.mutable.Map
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

  def deserializeObject(e: scala.xml.Node, depth: Int): Image = {
    val attribute = e.attributes.asAttrMap
    val actualType = Class.forName(attribute("actualType"))
    val declaredType = Class.forName(attribute("declaredType"))
    val fields = Map[String, (Image, Boolean)]()
    for (c <- e.child) 
      fields += deserializeField(c, depth)
    new Image(null, declaredType, depth, fields)
  }

  def deserializeField(e: scala.xml.Node, depth: Int)
  : (String, (Image, Boolean)) = {
    val attribute = e.attributes.asAttrMap
    val name = attribute("name")
    var declaredType = attribute("declaredType")
    try { declaredType = Class.forName(declaredType) }
    catch { case x: ClassNotFoundException => ; } // Retain attribute value
    for (c <- e.child) 
      println(c.getClass)
    ((name, (null, false)))
  } 

  // TODO Fix
  def describe(subject: Any): String = 
    subject match {
    case null => "null"
    case _ => subject.asInstanceOf[AnyRef].getClass().getCanonicalName() + "@" + subject.hashCode().toString()
    }
  
  def hash(subject: Any): Int = describe(subject).hashCode()
}

