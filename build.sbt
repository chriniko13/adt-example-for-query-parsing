name := "adt-example-for-query-parsing"

version := "0.1"

scalaVersion := Version.Scala

libraryDependencies ++= Seq(

  "log4j" % "log4j" % Version.Log4j,
  "org.slf4j" % "slf4j-simple" % Version.Slf4j,

  "io.spray" %% "spray-json" % Version.Spray,

  "org.scalatest" %% "scalatest" % Version.ScalaTest % "test"
)