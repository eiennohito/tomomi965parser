package ru.dijkstra.tomomi

import java.io.File
import java.net.{URL, URI}
import scalax.file.Path
import java.nio.file.OpenOption
import scalax.io.{Codec, StandardOpenOption}
import concurrent.{ExecutionContext, Future}

/**
 * @author eiennohito
 * @since 12.01.13 
 */
class Grabber(path: String) {
  // Config
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
  def loadUri(uri: String, fut : Future[File])(implicit ec: ExecutionContext) = {
    import scalax.io.JavaConverters._
    fut.map { file => {
      val f = Path(file)
      val inp = new URL(uri).asInput.chars("shift_jis")
      f.writeChars(inp)("utf-8")
      file
    }}
  }
}
