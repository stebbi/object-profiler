package profiler

import java.io.{BufferedWriter, File, FileOutputStream, OutputStream, OutputStreamWriter, Writer}


class ImageAsString(image: Image) {

  def asString(): String =
    image.subject match {
      case null => "null"
      case _: String => "\"" + image.subject.toString() + "\""
      case _ => image.subject.toString()
    }
}


class ClassAsString(clss: Class[_]) {

  def asString(): String = clss.getCanonicalName()
}


class SerializingProfile(profile: Profile) {
  
  implicit def viewAsString(image: Image) = new ImageAsString(image)
  implicit def classAsString(clss: Class[_]) = new ClassAsString(clss)

  def serialize(depth: Int = 1): scala.xml.Elem = 
    object_(profile.root, depth)
  
  def object_(image: Image, depth: Int): scala.xml.Elem = 
    <object 
      actualType={image.subject.getClass().asString}
      declaredType={image.declaredType.getClass().asString}>{
      image.visit(
        (name: String, image: Image, isProperty: Boolean) => {
          if (isProperty)
            property(name, image, depth)
          else
            field(name, image, depth)
        })
    }</object>

  def field(name: String, image: Image, depth: Int): scala.xml.Elem = 
    <field name={name} declaredType={image.declaredType.asString}>{
    if (0 < depth)
      if (image.isInScope) 
        object_(image, depth - 1)
      else 
        image.subject.toString
    }</field>
 
  def property(name: String, image: Image, depth: Int): scala.xml.Elem = 
    <property name={name} type={image.declaredType.asString}>{
    if (0 < depth)
      if (image.isInScope) 
        object_(image, depth - 1)
      else if (null != image.subject)
        image.subject.toString
      else ""
    }</property>
}


class SerializingDifference(difference: Difference) {

  implicit def viewAsString(image: Image) = new ImageAsString(image)
  implicit def classAsString(clss: Class[_]) = new ClassAsString(clss)
  implicit def serializing(d: Difference) = new SerializingDifference(d)

  def serialize(depth: Int): scala.xml.Elem = {
    difference match {
    case d: FieldDifference => apply(d)
    case d: Missing => apply(d)
    case d: Extra => apply(d)
    case d: Reconciliation => apply(d, depth)
    case _ => throw new UnsupportedOperationException()
    }
  }

  def apply(r: Reconciliation, depth: Int): scala.xml.Elem = 
    <object name={r.name}>
      <left 
        declared={r.left.declaredType.asString}
        type={r.left.actualType.asString}>
      </left>
      <right 
        declared={r.right.declaredType.asString}
        type={r.right.actualType.asString}>
      </right>{
      for (d <- r) 
        yield d.serialize(depth)
    }</object>

  def apply(d: FieldDifference): scala.xml.Elem = 
    <field name={d.name}>
      <left type={d.left.actualType.asString}>
        {d.left.asString}
      </left>
      <right type={d.right.actualType.asString}>
        {d.right.asString}
      </right>
    </field>

  def apply(d: Missing): scala.xml.Elem = 
    <left type={d.left.declaredType.asString}>{d.left.asString}</left>
  
  def apply(d: Extra): scala.xml.Elem = 
    <right type={d.right.declaredType.asString}>{d.right.asString}</right>
}


object serialize {

  implicit def profile(p: Profile) = new SerializingProfile(p)

  implicit def difference(d: Difference) = new SerializingDifference(d)

  implicit def stream2writer(s: OutputStream): Writer = 
    new BufferedWriter(new OutputStreamWriter(s))

  def apply(p: Profile, depth: Int): scala.xml.Elem = p.serialize(depth)
   
  def apply(p: Profile, depth: Int, out: OutputStream): Unit = {
    val w: Writer = out
    w.write(prettify(apply(p, depth))) 
  }

  def apply(p: Profile, depth: Int, path: String): Unit = 
    apply(p, depth, new File(path))

  def apply(p: Profile, depth: Int, out: File): Unit = 
    withWriter(out, writer => {
      writer.write(prettify(apply(p, depth))) }) 

  def apply(r: Reconciliation, depth: Int): scala.xml.Elem = 
    <reconciliation>{r.serialize(depth)}</reconciliation>

  def apply(r: Reconciliation, depth: Int, out: OutputStream): Unit = {
    val w: Writer = out
    w.write(prettify(apply(r, depth)))
  }
  
  def apply(r: Reconciliation, depth: Int, out: File): Unit =
    withWriter(out, writer => {
      writer.write(prettify(apply(r, depth))) })

  def withWriter(out: File, op: Writer => Unit) = {
    val w: Writer = new FileOutputStream(out)
    try { op(w) } finally { w.close() }
  }
}

 
