import collection.mutable
import java.io.File
import java.nio.charset.Charset
import java.util.Locale
import org.apache.commons.io.FileUtils
import scala.util.parsing.combinator.RegexParsers
import org.apache.http._
import client.HttpClient
import client.methods.{HttpGet, HttpPost}
import impl.client.DefaultHttpClient
import scala.collection.mutable.MutableList

object Parser extends RegexParsers {
  override type Elem = Char

}

object Grabber {
  // Config
  val path = """c:\dev\grabbing\"""
  val uriPrefix = """http://www.geocities.jp/tomomi965/ko-jien"""
  val struct =
    List(
      ("01", "a" , 20),
      ("02", "ka", 22),
      ("03", "sa", 19),
      ("04", "ta", 20),
      ("05", "na", 11),
      ("06", "ha", 17),
      ("07", "ma", 14),
      ("08", "ya", 6 ),
      ("08", "ra", 5 ),
      ("08", "wa", 2 )
    )

  def twoDigitNumber(i : Int) : String = {
    val str = i.toString
    str.length match {
      case 1 => "0" + str
      case 2 => str
      case _ => throw new IllegalArgumentException("i")
    }
  }
  def prepareUri(section : String, mora : String, index: Int): (String, String) = {
    val name = mora + twoDigitNumber(index) + ".html"
    (uriPrefix + section + """/""" + name, section + "_" + name)
  }
  def prepareFilePath(section : String, mora : String, index: Int): String = {
    val name = mora + twoDigitNumber(index) + ".html"
    path + section + "_" + name
  }
  def uriList = struct flatMap { x => (1 to x._3) map { index : Int => prepareUri(x._1, x._2, index) } }
  def fileList = struct flatMap { x => (1 to x._3) map { index : Int => prepareFilePath(x._1, x._2, index) } }
  def loadUri(uri: String, file : File) {
    val httpClientDefault1 : HttpClient = new DefaultHttpClient()
    val httpPost = new HttpGet(uri)
    httpPost.setHeader("Connection", "keep-alive")
    httpPost.setHeader("User-Agent", "Mozilla/5.0 (Windows NT 6.1; rv:9.0.1) Gecko/20100101 Firefox/9.0.1")
    httpPost.setHeader("Accept", " text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    httpPost.setHeader("Accept-Language", "en-us,en;q=0.5")
    val httpRespnse = httpClientDefault1.execute(httpPost)

    if(httpRespnse.getStatusLine().getStatusCode() == 200) {
     val in =  httpRespnse.getEntity().getContent()
      val html = new StringBuilder("")
      var b = new Array[Byte](1024*1024)
      while(in.read(b) != -1) {
        val str = new String(b, Charset.forName("shift_jis")).trim
        html.append(str)
        b = new Array[Byte](1024)
      }
      FileUtils.writeStringToFile(file, html.toString().replace("Shift_JIS","UTF-8"))
    }

  }
}

object Main {
  def main(args:Array[String]) {
    Grabber.uriList.reverse map {
      x =>
        val file = new File(Grabber.path + x._2)
        if (!file.exists()) {
          Grabber.loadUri(x._1, file)
        }
    }
    val sb = new StringBuilder(10 *1024 * 1024)
    Grabber.fileList foreach {
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
    val records = new File(Grabber.path + "results.txt")
    FileUtils.writeStringToFile(records, str)
  }
}