package ru.dijkstra.tomomi

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import scalax.file.Path


class TagExtractorSpec extends FreeSpec with ShouldMatchers {
  "something" - {
    "does work" in {
      val f = Path.fromString("E:/Temp/wap_soft/kowotaza/01_a05.html")
      val s = f.string
      //val bgs = Finder.begin(s)
      //bgs.foreach(x => {print("#"); println(x.data)})
    }

    "and parses title" in {
      val o = """百に千に（ももにちに） 色々に。様々に。 用例：<a href="../ko-jien01/a07.html#iki3">万葉</a>－３０５９「百に千に人は言ふとも」"""
      val pr = Normalizer.matchTitle(o)
      pr should equal (Some(("百に千に", "ももにちに")))
    }

    "with just kana too" in {
      val x = """ももにちに 色々に。様々に。 用例：<a href="../ko-jien01"""
      val pr = Normalizer.matchTitle(x)
      pr should equal (Some(("ももにちに", "ももにちに")))
    }

    "bold after" in {
      val s = "<B>着た切り雀</B>（きたきりすずめ）　着ている服"
      val b = Finder.boldAfter(s, 0)
      b should equal (true)
    }
  }
}
