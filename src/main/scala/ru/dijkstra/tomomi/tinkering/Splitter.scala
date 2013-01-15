package ru.dijkstra.tomomi.tinkering

import scalax.file.Path
import scalax.io.Codec
import net.liftweb.json.{Extraction, JsonParser, DefaultFormats}
import java.io.InputStreamReader
import ru.dijkstra.tomomi.{Normalizer, Entry}
import net.liftweb.json.JsonAST.JString
import util.matching.Regex.Groups

/**
 * @author eiennohito
 * @since 13.01.13 
 */

object Splitter {
  val re = """<a href="(.*?)">(.*?)</a>""".r
  def cleanlinks(s: String) = {
    re.replaceAllIn(s, p => p match {
      case Groups(ref, text) => text
    })
  }


  val fieldRE = " ([^ 。:：]{1,8})[:：]".r
  def main(args: Array[String]) {
    val path = Path.fromString(args(0))
    val cont = (path / "results.json")
    implicit val formats = DefaultFormats
    for (is <- cont.inputStream) {
      val rdr = new InputStreamReader(is, "utf-8")
      val jv = JsonParser.parse(rdr, false)
      val d = Extraction.extract[List[Entry]](jv)

      val lists = d.flatMap(x => fieldRE.findAllMatchIn(Normalizer.stripTags(x.raw)).flatMap {
        case Groups(g) => g :: Nil
        case _ => Nil
      })
      val aggr = lists.foldLeft(Map[String, Int]().withDefaultValue(0)){
        (m, x) => m.updated(x, m(x) + 1)
      }.toList.sortBy(-_._2)
      aggr.foreach { case (x, cnt) => println(s"$x -> $cnt")}
    }
  }
}
