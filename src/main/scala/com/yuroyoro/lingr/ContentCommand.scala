package com.yuroyoro.lingr
import scala.xml.Node

object ContentCommand extends Command with XMLCrawler with YahooApi {
  val command:String = "content"
  val usage:String = "<url> urlのタイトルと本文を抽出する"

  val url = "http://search.yahooapis.jp/WebSearchService/V2/webSearch?appid=%s&query=%s"

  def apply(commandMessage:CommandMessage):Option[String] =  {
    def formatResult(xml:Node) = "%s %s\n\n%s\n" format( xml \ "Title" text ,
      xml \ "Url" text, xml \ "Summary" text )

    Option(commandMessage.args.mkString(" ")).filter{ _.trim.nonEmpty }.flatMap{ word =>
      crawlXml(url format(apikey, urlEncode(word))){ xml =>
        (xml \\ "ResultSet" \\ "Result").headOption.map{ formatResult }
      }
    }
  }
}

