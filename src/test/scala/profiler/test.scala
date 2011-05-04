package profiler 

import org.scalatest.testng.TestNGSuite
import org.testng.Assert.assertEquals
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
  def testProfiler(): Unit = {
    prettify(report(FILE))
  }

  @Test
  def testReconcileSame(): Unit = {
    val comparison = reconcileFiles(FILE, FILE, 4)
    val report = serialize(comparison)
    val mismatched = report \\ "reconciliation" \\ "mismatched"
    assertTrue(mismatched.head.child.isEmpty)
    val matched = report \\ "reconciliation" \\ "matched" \\ "image"
    assertEquals(matched.length, 6)
  }

  @Test
  def testReconcileMismatchedTypes(): Unit = {
    val comparison = reconcileFiles(FILE, FIOS, 4)
    val report = serialize(comparison)
    val mismatched = report \\ "reconciliation" \\ "mismatched" \\ "difference"
    assertEquals(mismatched.length, 5)
    val matched = report \\ "reconciliation" \\ "matched" 
    assertTrue(matched.head.child.isEmpty)
  }

  @Test
  def testTree(): Unit = {
    val comparison = reconcileFiles(FILE, FILE2, 4)
    println(prettify(tree(comparison.left, 4)))
  }

  def main(args: Array[String]): Unit = {
  }
}
