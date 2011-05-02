
package object profiler {
  
  def describe(subject: Any): String = 
    subject match {
    case null => "null"
    case _ => 
        subject.asInstanceOf[AnyRef].getClass().getCanonicalName() + "@" + subject.hashCode().toString()
    }
  
  def hash(subject: Any): Int = describe(subject).hashCode()
}

