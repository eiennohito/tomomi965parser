package ru.dijkstra.tomomi

import java.io.File
import java.nio.charset.Charset

import org.apache.commons.io.FileUtils
import scala.util.parsing.combinator.RegexParsers
import org.apache.http._
import client.HttpClient
import client.methods.{HttpGet}
import impl.client.DefaultHttpClient
import scala.collection.mutable.MutableList

object Main {
  def main(args:Array[String]) {
    if (args.length != 1) {
      throw new Exception("Specify path as argument")
    }
    val path = args(0)

    val g = new Grabber(path)
    g.uriList.reverse map {
      x =>
        val file = new File(path + x._2)
        if (!file.exists()) {
          g.loadUri(x._1, file)
        }
    }
    val sb = new StringBuilder(10 *1024 * 1024)
    g.fileList foreach {
      x =>
        val file = new File(x)
        assert(file.exists())
        val html = FileUtils.readFileToString(file)
          .replace("\n", "")
        val regexp_wordbegin = """(<B>|<B><br>?)\s*・\S+?</B>（.+?）""".r
        val regexp_docend = """<B><a href="../(index2|aihara?).html" target="_top">""".r

        val results = regexp_wordbegin.findAllMatchIn(html)
        val endof = regexp_docend.findAllMatchIn(html)

        val points = new MutableList[Int]()

        results foreach { x => points += x.start }
        endof take(1) map { x => points += x.start }

        val intervals = new MutableList[(Int, Int)]()

        /*
        (0 to points.length - 1 - 1)
          .foreach { x => intervals += ((points.get(x).get, points.get(x + 1).get)) }
          */
        if (!points.isEmpty) {
          (points.take(points.length), points.tail).zipped.foreach((x, y) => intervals += ((x,y)))
          val records = intervals map {
            int =>
                 val frm = int._1
                 val to = int._2
                 html.substring(frm, to)
                   .replaceAll("""<br>\s*""", "")
                   .replaceAll("""<a href=".+?">(.+?)</a>""","""[LINKWORD]$1[/LINKWORD]""")
                   .replaceAll("""<font.*?>""", "")
                   .replaceAll("""</font.*?>""", "")
                   .replaceAll("""<b>(.+?)</b>""", """$1""")
                   .replaceAll("""<a name=".+?" id=".+?"></a>−−−−−−−.+?−−−−−−−""", "")
                   .replaceFirst("""<B>・(.+?)</B>（(.+?)）""", """[WRITING]$1[/WRITING][READING]$2[/READING]""")
                   .replaceAll("""<B>""", "")
                   .replaceAll("""</B>""", "")
                   .replaceAll("""《四熟》""", "[YOJIJUKUGO]")
                   .replaceAll("""《四熟・囃し言葉》""", "[YOJIJUKUGO][HAYASHIKOTOBA]")
                   .replaceAll("""《囃し言葉》""", "[HAYASHIKOTOBA]")
                   .replaceAll("""《俗語》""", "[SLANG]")
                   .replaceAll("""《四熟・仏教用語》""", "[YOJIJUKUGO][BUDDHISTSHIT]")
                   .replaceAll("""《仏教用語》""", "[BUDDHISTSHIT]")
          }
          records foreach { x => sb.append("\n---" + x) }
        }
    }
    val str = sb.toString()
    val records = new File(path + "results.txt")
    FileUtils.writeStringToFile(records, str)
  }
}
