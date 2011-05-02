package profiler


object serialize {
  
  def apply(profile: Profile, depth: Int = 1): scala.xml.Elem = 
    return <profile>{ for (image <- profile) 
      yield serialize(image) 
  }</profile>
  
  def apply(image: Image): scala.xml.Elem = 
    return <image id={describe(image.subject)}>{ 
      for (field <- image.visit(applyx))
        yield field 
    }</image>
      
  protected def applyx(name: String, value: Image): scala.xml.Elem = 
    if (value.isInScope())
      return <field name={name} refid={describe(value.subject)}/>
    else
      return <field name={name} class={value.declaredType.getCanonicalName}>{asString(value)}</field>

  def apply(diff: Difference): scala.xml.Elem =
    diff match {
    case d: FieldDifference => apply(d)
    case d: Missing => apply(d)
    case d: Extra => apply(d)
    case _ => throw new UnsupportedOperationException()
  }

  def apply(diff: FieldDifference): scala.xml.Elem = 
    <difference>
      <left declaredType={asString(diff.left.declaredType)}>
        {asString(diff.left)}
      </left>
      <right declaredType={asString(diff.right.declaredType)}>
        {asString(diff.right)}
      </right>{ 
      for ((name, (left, right)) <- diff.different) yield 
      <field name={name}>
        <left declaredType={asString(left.declaredType)}>{asString(diff.left)}</left>
        <right declaredType={asString(right.declaredType)}>{asString(diff.right)}</right>
      </field>}
    </difference>

  def apply(reco: Reconciliation): scala.xml.Elem = 
    <reconciliation left={asString(reco.left.root)} right={asString(reco.right.root)}>
      <matched>{ 
      for (hit <- reco.hits) 
        yield serialize(hit) 
      }</matched>
      <mismatched>{ 
      for (difference <- reco.misses) 
        yield serialize(difference) 
      }</mismatched>
    </reconciliation>

  def apply(diff: Missing): scala.xml.Elem = 
    <missing>{diff.left.subject}</missing>
  
  def apply(diff: Extra): scala.xml.Elem = 
    <extra>{diff.right.subject}</extra>

  def asString(value: Image): String =
    value.subject match {
      case null => "null"
      case _: String => "\"" + value.subject.toString() + "\""
      case _ => value.subject.toString()
    }

  def asString(clss: Class[_]): String = 
    clss.getCanonicalName()
}
