package ru.dijkstra.tomomi

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import scalax.file.Path
import tinkering.EntryParser


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

    "dunno" in {
      val s = "名誉挽回</B>（めいよばんかい) <font color=\"#999999\">《四熟》</font> 一度失った名誉や信用を取り戻すこと｡ 類：●<a href=\"ma12.html#bottom\">面目一新</a>●<a href=\"ma12.html#bottom\">面目躍如</a><B>"
      val e = Normalizer.normalize(Sent(s, 0, s.length))
      println(e)
      e should not be (None)
    }

    "body parser" in {
      val part = "悲しみのあまり痩せ細り、骨ばかりになる。父母の死などで非常に悲しむことの喩え。 類：●毀瘠骨立●形鎖骨立●羸瘠骨立 出典：「<a href=\\\"../kotowaza04/4-19-6.html\\\">世説新語</a>－徳行・一」「王戎雖不備礼、而哀毀骨立」 例：「哀毀骨立の風体（ふうてい）」 ★「哀毀」は、悲しみのあまり身体を壊す、また、痩せ細ること。「骨立」は、痩せて骨ばかりになること。"
      EntryParser.parseBody(part)
    }
  }
}
