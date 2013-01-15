package ru.dijkstra.tomomi.tinkering

import collection.mutable.ListBuffer
import org.apache.commons.lang3.StringUtils

/**
 * @author eiennohito
 * @since 15.01.13 
 */

case class Body(meaning: String, links: List[Links], info: List[Info])
case class Links(kind: String, data: List[String])
case class Info(name: String, content: String)

object EntryParser {
  implicit class NullSupport[T <: AnyRef](val x: T) extends AnyVal {
    def or[T1 >: T](y: => T1): T1 = if (x == null) y else x
  }

  val fieldRE = "([ 。]([^ :：]{1,8})[:：])|((★)[^ :：]{7})".r(null, "info", null, "star")

  val numberedRe = """[０-９][．\.]""".r
  def checkNumber(s: String) = numberedRe.findFirstIn(s).isDefined

  val linkMarks = Set("類", "反", "同")

  val linkre = "[●■]".r
  def parseLinks(s: String): List[String] = {
    linkre.split(s).toList.map(StringUtils.strip(_, "：■ ●")).filter(_.length > 1)
  }

  def handleBraces(s: String): String = {
    val s1 = StringUtils.trim(s)
    if (s1.startsWith("「") && s1.endsWith("」") && !s1.contains("」「")) {
      s1.substring(1, s1.length - 1)
    } else s1
  }

  def parseBody(s: String): Body = {
    val items = fieldRE.findAllMatchIn(s)
    val begins = List(0) ++ items.map { _.start } ++ List(s.length)
    val cont :: rest = begins.sliding(2).map { case x :: y :: Nil => s.substring(x, y) }.toList
    val info = new ListBuffer[Info]
    val links = new ListBuffer[Links]
    rest.foreach { e =>
      fieldRE.findFirstMatchIn(e) match {
        case None => println(s"! -> $e")
        case Some(g) => {
          g.group(2) or g.group(4) match {
            case m if linkMarks.contains(m) => { links += Links(m, parseLinks(e.substring(g.end(2)))) }
            case x => {
              val pos = if (g.start(1) == -1) g.end(4) else g.end(1)
              info += Info(x, handleBraces(e.substring(pos)))
            }
          }
        }
      }
    }
    Body(cont, links.result(), info.result())
  }

  def parse(s: String): List[Body] = {
    val itms = numberedRe.split(s)
    itms.toList.filter(_.length > 2).map(parseBody)
  }
}
