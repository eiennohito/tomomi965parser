package ru.dijkstra.tomomi

import collection.mutable.ListBuffer

/**
 * @author eiennohito
 * @since 12.01.13 
 */

case class Sent(src: String, beg: Int, end: Int) {
  def data = src.substring(beg, end)
}

object Finder {
  implicit class MegaInt(val x: Int) extends AnyVal {
    def xmax(o: Int): Int = {
      if (x == -1)
        return o
      if (o == -1)
        return x
      if (x > o) x else o
    }

    def xmin(o: Int): Int = {
      if (x == -1)
        return o
      if (o == -1)
        return x
      if (x > o) o else x
    }
  }

  val bre = """・""".r
  val ere = """<br>""".r

  val bold = List("b", "B", "strong", "STRONG")
  val open = bold map { c => s"<$c>" }
  val close = bold map { c => s"</$c>" }

  def boldAfter(s: String, pos: Int): Boolean = {
    open.find(t => s.substring(pos).startsWith(t)).isDefined
  }

  def insideBoldTag(s: String, pos: Int) = {
    val bg = 0 xmax (pos - 200)
    val ss = s.substring(bg, pos)
    val beg = open.map(t => ss.lastIndexOf(t)).reduce(_ xmax _)
    val bi = (beg + bg) xmax 0
    val end = close.map(t => s.indexOf(t, bi)).reduce(_ xmin _)
    (beg > 0 && end >= pos) || boldAfter(s, pos)
  }

  val exceptions = List(
    "胸を冷やす",
    "無明世界"
  )

  def isException(s: String): Boolean = {
    exceptions.find(i => s.startsWith(i)).isDefined
  }

  def pos(s: String, pos: Int) = {
    bre.findFirstMatchIn(s.substring(pos))  match {
      case None => {
        println(s"refusing next from $pos of ${s.length}")
        None
      }
      case Some(m) => {
        val beg = pos + m.end
        ere.findFirstMatchIn(s.substring(beg)) match {
          case None => {
            println(s"couldn't find end: pos: $pos, data:\n${s.substring(beg, s.length min (pos + 100))}")
            None
          }
          case Some(t) => {
            if (insideBoldTag(s, beg) || isException(s.substring(beg)) )
              Some(Sent(s, beg, beg + t.start))
            else {
              val st = 0 max (beg - 10)
              val end = s.length min (beg + t.start + 10)
              val perc = (beg + t.end).toDouble / s.length
              if (perc < 0.97) {
                println(s"don't have bold here ($perc)%: \n${s.substring(st, end)}")
              }
              None
            }
          }
        }
      }
    }
  }

  def begin(s: String) = {
    val bldr = new ListBuffer[Sent]()
    val start = s.indexOf("<table") xmax s.indexOf("<TABLE")
    assert(start != -1)
    var x = pos(s, start)
    while(x.isDefined) {
      val i = x.get
      bldr += i
      x = pos(s, i.end)
    }
    bldr.result
  }
}
