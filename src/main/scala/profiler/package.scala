package object profiler {

  import java.io.OutputStream

  def profile(subject: AnyRef, depth: Int): Profile = 
    new Profiler(subject, depth).profile()

  def reconcile(left: AnyRef, right: AnyRef, depth: Int): Reconciliation = 
    new Reconciliation(profile(left, depth), profile(right, depth), depth)

  def prettify(xml: scala.xml.Elem): String = 
    new scala.xml.PrettyPrinter(80, 2).format(xml)

  def report(subject: AnyRef, depth: Int, out: OutputStream) = 
    serialize(profile(subject, depth), depth, out)

  def report(left: AnyRef, right: AnyRef, depth: Int, out: OutputStream) = 
    serialize(reconcile(left, right, depth), depth, out)

  // TODO Fix
  def describe(subject: Any): String = 
    subject match {
    case null => "null"
    case _ => subject.asInstanceOf[AnyRef].getClass().getCanonicalName() + "@" + subject.hashCode().toString()
    }
  
  def hash(subject: Any): Int = describe(subject).hashCode()
}

