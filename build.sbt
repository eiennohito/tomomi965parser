organization := "ru.dijkstra"

scalaVersion := "2.10.0"

name := "tomomi"

libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.2.2"

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.1" exclude ("javax.transaction", "jta")

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
