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
import scala.concurrent.forkjoin.{ForkJoinTask, ForkJoinPool}
import scala.concurrent.{Await, Future, ExecutionContext}
import net.liftweb.json.{DefaultFormats, Extraction}

object Main {
  def main(args:Array[String]) {
    if (args.length != 1) {
      throw new Exception("Specify path as argument")
    }
    val path = args(0)

    val g = new Grabber(path)
    implicit val ec = ExecutionContext.fromExecutor(new ForkJoinPool(8))

    val fls = g.uriList map {
      case (uri, fname) =>
        val file = new File(path + fname)
        val fut = Future.successful(file)
        if (!file.exists()) {
          val f = g.loadUri(uri, fut)
          f.onFailure {
            case e => e.printStackTrace()
          }
          f
        } else {
          fut
        }
    }
    import scala.concurrent.duration._
    Await.ready(Future.sequence(fls), 10 minutes)
    val data = g.fileList flatMap {
      x =>
        val file = new File(x)
        assert(file.exists())
        val html = FileUtils.readFileToString(file)

        val all = Finder.begin(html)
        println(s"from file ${file.getName} got ${all.size} items")

        all.flatMap(s => {
          Normalizer.normalize(s)
        })
    }
    val jv = Extraction.decompose(data)(DefaultFormats)
    import net.liftweb.json
    val str = json.pretty(json.render(jv))
    val records = new File(path + "results.json")
    FileUtils.writeStringToFile(records, str)
  }
}
