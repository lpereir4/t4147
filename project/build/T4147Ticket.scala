import sbt._

class T4147Ticket(info: ProjectInfo) extends DefaultProject(info) {

  val scalaCheck = "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.9"
}

