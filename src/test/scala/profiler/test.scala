package profiler 

import org.scalatest.testng.TestNGSuite
import org.testng.Assert.assertTrue
import org.testng.annotations.Test


class Tests extends TestNGSuite {

  val FILE  = new java.io.File("/tmp/test.txt")
  val FILE2 = new java.io.File("/tmp/test2.txt")
  val FIOS  = new java.io.FileInputStream("/tmp/test.txt") 
 
  def profile(subject: AnyRef, depth: Int): Profile = 
    new Profiler(subject, depth).profile()
    
  def report(subject: AnyRef): scala.xml.Elem = 
    new Profiler(subject, 4).report()
    
  def prettify(xml: scala.xml.Elem): String = 
    new scala.xml.PrettyPrinter(80, 2).format(xml)
  
  def reconcileFiles(left: AnyRef, right: AnyRef, depth: Int): Reconciliation = 
    Reconciliation(profile(left, depth), profile(right, depth))
  
  @Test
  def test(): Unit = {
    println(prettify(report(FILE)))
    val comparison = reconcileFiles(FILE, FIOS, 4)
    println(prettify(serialize(comparison)))
    assertTrue(true)
  }

  def main(args: Array[String]): Unit = {
    test()
  }
}
