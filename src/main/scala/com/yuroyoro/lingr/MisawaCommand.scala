package com.yuroyoro.lingr

import scala.io.Codec

object MisawaCommand extends Command with Crawler {
  val command:String = "misawa"
  val usage:String = "<eid> 地獄のミサワ http://jigokuno.com/?eid=<eid>の画像を取ってくる。引数を省略したら最新の1件をとる。"

  override val codec:Codec = Codec("euc-jp")

  val h2 = """<h2>(.*)</h2>""".r
  val img1= """<img .*src="([^"]+)" .*class="pict" .*>""".r
  val img2= """<img .*class="pict".*src="([^"]+)" .*>""".r

  def apply(commandMessage:CommandMessage):Option[String] = {
    commandMessage.args.headOption.flatMap{ eid =>
      crawlMisawa("http://jigokuno.com/?eid=%s" format eid)
    }.orElse{
      crawlMisawa("http://jigokuno.com/")
    }
  }

  def crawlMisawa(url:String)=  crawl(url){html =>
    Some(Seq(
      h2.findFirstMatchIn(html).map{_.group(1)}.getOrElse(""),
      img1.findFirstMatchIn(html).map{_.group(1)}.orElse{
        img2.findFirstMatchIn(html).map{_.group(1)}}.getOrElse("")
    ).filter{_.trim.nonEmpty}.mkString("\n"))
  }

}

