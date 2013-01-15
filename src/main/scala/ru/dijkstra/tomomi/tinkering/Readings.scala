package ru.dijkstra.tomomi.tinkering

import scalax.io.Codec

/**
 * @author eiennohito
 * @since 13.01.13 
 */

object Readings {

  val data = {
    import scalax.io.JavaConverters._
    val src = this.getClass.getClassLoader.getResourceAsStream("readings.txt")
    val lines = src.asInput.lines()(Codec.UTF8).toList
    lines.map (l => {
      val wrds = l.split(",")
      wrds(0) -> wrds.drop(1).toList
    }).toMap
  }

  def filterLength(s: String) = {
    val parts = s.split("・")
    filterParts(parts)
  }


  def filterParts(parts: Array[String]): Boolean = {
    val l0 = parts.head.length.toFloat
    parts.drop(1).forall(i => {
      val ratio = l0 / i.length
      (ratio > 0.9 && ratio < 1.1) || (Math.abs(l0 - i.length) == 1)
    })
  }

  def apply(w: String, r: String) = {
    if (r.contains("・")) {
      data.get(w) match {
        case Some(s) => s
        case None => {
          val arr = r.split("・")
          if (filterParts(arr)) {
            arr.toList
          } else {
            println(s"#Error in $w -> $r")
            arr(0)
          }
        }
      }
    } else {
      r :: Nil
    }
  }

}
