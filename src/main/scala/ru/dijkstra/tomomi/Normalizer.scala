package ru.dijkstra.tomomi

import net.liftweb.json.DefaultFormats
import org.apache.commons.lang3.StringUtils
import collection.mutable.ListBuffer
import tinkering.{EntryParser, Body}
import util.matching.Regex.{Match, Groups}
import util.parsing.combinator.RegexParsers
import java.util.regex.Pattern
import util.parsing.input.CharSequenceReader
import annotation.tailrec

/**
 * @author eiennohito
 * @since 13.01.13 
 */

case class Entry(
  writing: String,
  reading: String,
  variant: Option[String],
  tags: List[String],
  body: List[Body],
  raw: String
)

object Normalizer {

  val boldre = "</?([bB]|strong|STRONG)>".r
  def stripBold(s: String) = {
    boldre.replaceAllIn(s, "")
  }

  val fontre = "</?font.*?>".r
  def stripFontTags(s: String) = fontre.replaceAllIn(s, "")

  def stripTags(s: String) = {
    stripBold(stripFontTags(s))
  }

  val wsre = "[\\s　 ]+".r
  def stripWs(s: String) = {
    wsre.replaceAllIn(s, " ")
  }

  def jv(e: Entry) = {
    import net.liftweb.json.Extraction._
    implicit val formats = DefaultFormats
    decompose(e)
  }

  object Tags {

    val map: String => List[String] = Map(
      "四熟" -> "4j",
      "囃し言葉" -> "hayashi",
      "俗語" -> "vulg",
      "仏教用語" -> "buhdda",
      "四塾" -> "4j",
      "略" -> "abbr",
      "仏教語" -> "buhdda",
      "季" -> "season",
      "株式用語" -> "stocks",
      "俗" -> "vulg",
      "秋" -> "autumn",
      "冬" -> "winter",
      "相場用語" -> "trade",
      "仏経用語" -> "buhdda",
      "故事成語" -> "hist",
      "法律用語" -> "legal",
      "仏語" -> "buhdda",
      "造語" -> "artif",
      "哲学用語" -> "philisiphy",
      "相場" -> "trade",
      "精神医学" -> "psychology",
      "ギリシア神話" -> "greek"
    ).map { case (a, b) => a -> List(b)}
    .withDefault(c => {println(s"$c is not in list"); Nil})

    def unapply(s: Match): Option[List[String]] = {
      Some(s.group(1).split("・").toList.flatMap(map))
    }
  }


  val tagre = "《(.*?)》".r
  def separateTags(s: String) = {
    val tags = new ListBuffer[String]
    val rest = tagre.replaceSomeIn(s, {
      case Tags(t) => tags ++= t; Some("")
    })
    (tags.result(), rest)
  }

  object X extends RegexParsers {

    override def skipWhitespace = false

    implicit def jregexParser(pat: Pattern): Parser[String] = {
      new Parser[String] {
        def apply(in: Input) = {
          val source = in.source
          val offset = in.offset
          val start = handleWhiteSpace(source, offset)
          val mat = pat.matcher(source.subSequence(start, source.length()))
          if (mat.lookingAt()) {
            Success(mat.group(0), in.drop(start + mat.end() - offset))
          } else {
            Failure("string matching regex `" + pat.toString + "` wasn't found", in.drop(start - offset))
          }
        }
      }
    }

    def content(p: Parser[_]): Parser[String] = new Parser[String] {
      def apply(in: Input) = {
        val r = p(in)
        r match {
          case Success(_, i1) => {
            val o1 = in.offset
            val o2 = i1.offset
            Success(in.source.subSequence(o1, o2).toString, i1)
          }
          case Failure(msg, in) => Failure(msg, in)
          case Error(msg, in) => Error(msg, in)
        }
      }
    }

    def codeUnits(f: Int => Boolean): Parser[String] = new Parser[String] {
      def apply(in: X.Input) = {
        val src = in.source
        @tailrec
        def items(st: Int, end: Int): ParseResult[String] = {
          if (in.atEnd) {
            return Failure("Should not be at end", in.drop(end - st))
          }
          val c1 = src.charAt(end)
          val e = if (Character.isHighSurrogate(c1)) {
            if (src.length() <= end) {
              return Failure("Should not be at end, prev is surrograte char", in.drop(end - st))
            }
            val c2 = src.charAt(end + 1)
            if (!Character.isLowSurrogate(c2)) {
              return Failure("Low surrogate should be after low surrogate", in.drop(end - st))
            }
            Character.toCodePoint(c2, c1)
          } else {
            c1.toInt
          }
          if (f(e)) {
            items(st, end + (if (e == c1) 1 else 2))
          } else {
            if (st == end) {
              Failure("Didn't match anything", in)
            } else {
              Success(src.subSequence(st, end).toString, in.drop(end - st))
            }
          }
        }
        items(in.offset, in.offset)
      }
    }

    def kana: Parser[String] =
      codeUnits(UnicodeUtil.isKana)

    def kanji: Parser[String] =
      codeUnits(UnicodeUtil.isKanji)

    def reading = content(rep(kana | "[ゝ・,、。,~～]".r))

    def writing = content(rep(kanji | kana | "[ゝ々・、。～]".r))

    def title = complexTitle | simpleWritingReading


    def complexTitle: Normalizer.X.Parser[(String, String)] = {
      (writing ~ ("[\\(（]".r ~> reading <~ "[）\\)]".r)) ^^ {
        case a ~ b => (a, b)
      }
    }

    def simpleWritingReading: Normalizer.X.Parser[(String, String)] = {
      reading ^^ {case p => (p, p) }
    }
  }

  def matchTitle(s: String) = {
    val si = new CharSequenceReader(s)
    X.title(si) match {
      case X.Success(i, _) => Some(i)
      case _ => None
    }
  }

  object XHash {
    def unapply(s: String): Option[Int] = {
      if (!s.startsWith("#")) {
        return None
      }
      val s1 = s.substring(1)
      try {
        Some(s1.toInt)
      } catch {
        case _ => None
      }
    }
  }

  val entre = "&([#0-9a-zA-Z]+);".r
  def entities(s: String) = {
    entre.replaceSomeIn(s, {
      case Groups(m) => m match {
        case "lt" => Some("<")
        case "gt" => Some(">")
        case "nbsp" => Some(" ")
        case "quot" => Some("\"")
        case XHash(c) => Some(new String(Character.toChars(c)))
      }
    })
  }


  val varre = "［[=＝](.+?)］".r
  def findVar(s: String): Option[String] = {
    varre.findFirstMatchIn(s) flatMap {
      case Groups(v) => Some(stripTags(v))
      case _ => None
    }
  }

  def normalize(s: Sent) = {
    import Finder.MegaInt
    val raw = StringUtils.strip(stripWs(entities(s.data)), " ")
    val woHtml = stripTags(raw)
    val (tags, clean) = separateTags(woHtml)
    matchTitle(clean) match {
      case Some((w, r)) => {
        val idx = clean.indexOf(" ") xmin clean.indexOf("目配せしをて意思を通ずる。")
        if (idx == -1) {
          println(s"wtf is this: $clean")
          Nil
        } else {
          val cont = StringUtils.strip(clean.substring(idx))
          val vnt = findVar(raw)
          List( Entry(w, r, vnt, tags, EntryParser.parse(cont), raw) )
        }
      }
      case _ => println(s"#ERROR:\n$$$clean\n%$raw"); Nil
    }
  }
}
