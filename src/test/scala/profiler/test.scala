package profiler 

import org.scalatest.testng.TestNGSuite
import org.testng.Assert.assertEquals
import org.testng.Assert.assertTrue
import org.testng.annotations.Test


object fixture {

  val FILE  = new java.io.File("/tmp/test.txt")
  val FILE2 = new java.io.File("/tmp/test2.txt")
  val FILE3  = new java.io.File("/tmp/tmp/test.txt")
  val FIOS  = new java.io.FileInputStream("/tmp/test.txt") 
}


class TestProfiles extends TestNGSuite {

  /** Profiles a file object and serializes XML to stdout. */
  // @Test
  // def testProfiler(): Unit = profiler.report(fixture.FILE, 3)
}


class TestToFile extends TestNGSuite {

  /** Profiles a file object and serializes the XML to /tmp/out.xml */
  @Test
  def test(): Unit = 
    profiler.report(fixture.FILE, 3, "/tmp/out.xml")
}


class TestFromFile extends TestNGSuite {

  @Test 
  def test(): Unit = {
    val p = profiler.load("/tmp/out.xml")
  }
}


class TestReconciliation extends TestNGSuite {

  @Test
  def testReconcileSame(): Unit = {
    val depth = 4
    val comparison = reconcile(fixture.FILE, fixture.FILE, depth)
    val report = serialize(comparison, depth)
    println(prettify(report))
  }

  @Test
  def testReconcileMismatchedTypes(): Unit = {
    val depth = 4
    val comparison = reconcile(fixture.FILE, fixture.FIOS, depth)
    val r = serialize(comparison, depth)
    println(prettify(r))
  }

  @Test
  def testTree(): Unit = {
    val d = 4
    val comparison = reconcile(fixture.FILE, fixture.FILE3, d)
    val s = new java.io.FileOutputStream("/Users/stebbi/Scratch/out.xml")
    serialize(comparison, d, s)
  }
}
