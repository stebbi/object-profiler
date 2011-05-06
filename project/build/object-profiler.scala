
import sbt.{DefaultProject, ProjectInfo, ScalaToolsSnapshots}


class objectprofiler(info: ProjectInfo) extends DefaultProject(info) {
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.3"
  val testng = "org.testng" % "testng" % "6.0.1"
}
