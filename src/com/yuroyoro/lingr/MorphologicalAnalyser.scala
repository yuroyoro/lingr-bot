package com.yuroyoro.lingr
/**
 * Yahoo形態素解析APIを利用して形態素解析を行うObject
 */
object MorphologicalAnalyserCommand extends Command {
  import java.net.{URL, URLEncoder}
  import scala.xml._
  import scala.io.{Source, Codec}

  val apikey = "E5JE6uuxg64mzybiFvlbeXLmDyw3K1f.Kpj0D.W5JMQdXdMB98muWuy9PUqGOLiFIRmuplc-"
  val url = "http://jlp.yahooapis.jp/DAService/V1/parse?appid=%s&sentence=%s"

  val command:String = "ma"
  val usage:String = "<args> argsの文章をYahoo形態素解析APIに渡す"
  def apply(commandMessage:CommandMessage):String =  {
    implicit val codec = Codec.UTF8
    Source.fromURL( new URL(
      url.format( apikey, URLEncoder.encode(commandMessage.args.mkString , "utf-8")))
    ).mkString
  }
}

