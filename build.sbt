name := "Test"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies += "com.crealytics" %% "spark-excel" % "0.9.14"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

val sparkVersion = "2.2.0"

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % "2.2.0",
  "org.apache.spark" %% "spark-sql" % "2.2.0"
)
