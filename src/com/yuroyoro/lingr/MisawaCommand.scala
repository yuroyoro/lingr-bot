package com.yuroyoro.lingr

import java.net.URL
import scala.io.{Source, Codec}
import scala.util.matching.Regex

object MisawaCommand extends Command {
  val command:String = "misawa"
  val usage:String = "<eid> 地獄のミサワ http://jigokuno.com/?eid=<eid>の画像を取ってくる。引数を省略したら最新の1件をとる。"

  val h2 = """<h2>(.*)</h2>""".r
  val img1= """<img .*src="([^"]+)" .*class="pict" .*>""".r
  val img2= """<img .*class="pict".*src="([^"]+)" .*>""".r

  def apply(commandMessage:CommandMessage):String = {
    commandMessage.args.headOption.map{ eid =>
      crawlMisawa("http://jigokuno.com/?eid=%s" format eid)
    }.getOrElse{
      crawlMisawa("http://jigokuno.com/")
    }
  }

  def crawl(url:String)(f:String => String) = {
    implicit val codec = Codec("euc-jp")
    val src = Source.fromURL(new URL(url))
    val html = src.getLines.mkString("\n")
    f(html)
  }

  def crawlMisawa(url:String)=  crawl(url){html =>
    Seq(
      h2.findFirstMatchIn(html).map{_.group(1)}.getOrElse(""),
      img1.findFirstMatchIn(html).map{_.group(1)}.orElse{
        img2.findFirstMatchIn(html).map{_.group(1)}}.getOrElse("")
    ).filter{_.trim.nonEmpty}.mkString("\n")
  }

}

