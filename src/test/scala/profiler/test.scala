package profiler 

import org.scalatest.testng.TestNGSuite
import org.testng.Assert.assertEquals
import org.testng.Assert.assertTrue
import org.testng.annotations.Test


class Tests extends TestNGSuite {

  val FILE  = new java.io.File("/tmp/test.txt")
  val FILE2 = new java.io.File("/tmp/test2.txt")
  val FILE3  = new java.io.File("/tmp/tmp/test.txt")
  val FIOS  = new java.io.FileInputStream("/tmp/test.txt") 
 
  @Test
  def testProfiler(): Unit = serialize(profile(FILE, 3), 3)

  @Test
  def testReconcileSame(): Unit = {
    val depth = 4
    val comparison = reconcile(FILE, FILE, depth)
    val report = serialize(comparison, depth)
    println(prettify(report))
  }

  @Test
  def testReconcileMismatchedTypes(): Unit = {
    val depth = 4
    val comparison = reconcile(FILE, FIOS, depth)
    val r = serialize(comparison, depth)
    println(prettify(r))
  }

  @Test
  def testTree(): Unit = {
    val d = 4
    val comparison = reconcile(FILE, FILE3, d)
    val s = new java.io.FileOutputStream("/Users/stebbi/Scratch/out.xml")
    serialize(comparison, d, s)
  }
}
