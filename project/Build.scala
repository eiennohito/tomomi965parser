package ru.dijkstra.tomomi

import sbt._
import Keys._


/**
 * @author eiennohito
 * @since 12.01.13 
 */


class TomomiBuild extends Build {
  lazy val tomomi = Project(id = "tomomi", base = file("."))
}
