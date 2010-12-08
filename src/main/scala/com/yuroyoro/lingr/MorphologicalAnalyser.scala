package com.yuroyoro.lingr
/**
 * Yahoo形態素解析APIを利用して形態素解析を行うObject
 */
object MorphologicalAnalyserCommand extends Command with XMLCrawler with YahooApi{
  import scala.xml._

  val url = "http://jlp.yahooapis.jp/DAService/V1/parse?appid=%s&sentence=%s"

  val command:String = "ma"
  val usage:String = "<args> argsの文章をYahoo形態素解析APIに渡す"

  def apply(commandMessage:CommandMessage):Option[String] =  {
    crawlXml( url.format( apikey, urlEncode(commandMessage.args.mkString(" ")))){xml =>
      Some(xml.mkString)
    }
  }
}

