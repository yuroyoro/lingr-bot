package com.yuroyoro.lingr

import java.util.logging.Logger
import java.net.{URL, URLEncoder, URLDecoder}
import scala.util.Random
import scala.io.{Source, Codec}
import scala.xml.{XML, Node, NodeSeq}

import com.yuroyoro.util.io.PropertiesLoader
import com.yuroyoro.util.net._

case class Message(nickname:String, text:String, room:String)
case class CommandMessage(nickname:String, text:String, room:String, command:String, args:Seq[String])

object Bot {

  val messagePrefix = '&'
  val commands:Seq[Command]= Seq(
    HelpCommand,
    MisawaCommand,
    GyopaaaCommand,
    // MorphologicalAnalyserCommand,
    TWadaCommand,
    WikiPediaCommand,
    ContentCommand,
    // HatebuCommand,
    // LunchCommand,
    // DictCommand,
    EnochCommand,
    LucifelCommand,
    PingCommand,
    BijinTokeiCommand,
    BijoKoyomiCommand
  )

  def apply(message:Message):Seq[String] = {
    message.text.lines.toList.filter{ s =>
      s.startsWith( messagePrefix.toString )
    }.flatMap{ s =>
      val cs::args = s.split("\\s").toList
      val cmd = cs.dropWhile(messagePrefix==)
      val commandMessage = CommandMessage( message.nickname,
        message.text, message.room, cmd, args)

      commands.filter{ c =>
        c.matchCommand_?(commandMessage)
      }.flatMap{ c => c(commandMessage)}
    }
  }
}

trait Command {
  def log(msg:String) = Logger.getLogger(classOf[LingrBotFilter].getName).info(msg)

  val command:String
  val usage:String
  def apply(commandMessage:CommandMessage):Option[String]
  def matchCommand_?(commandMessage:CommandMessage) =
    commandMessage.command == command
}

trait RandomWordCommand extends Command {
  val messages:Seq[String]

  def apply(commandMessage:CommandMessage):Option[String] =
    Some(messages( Random.nextInt(messages.size)))
}

trait Crawler {
  val codec:Codec = Codec.UTF8

  def urlEncode(s:String) = URLEncoder.encode(s, codec.name)

  def crawl(url:String)(f:String => Option[String]):Option[String] = {
    implicit val cd = codec
    val src = Source.fromURL(new URL(url))
    if(src.nonEmpty){
      val html = src.mkString
      f(html)
    }else {None}
  }
}

trait XMLCrawler extends Crawler {

  def crawlXml(url:String)(f:NodeSeq => Option[String]) = {
    crawl(url){ s => f(XML.loadString(s)) }
  }
}

trait YahooApi {
  val apikey = "E5JE6uuxg64mzybiFvlbeXLmDyw3K1f.Kpj0D.W5JMQdXdMB98muWuy9PUqGOLiFIRmuplc-"
}

trait TwitterApi extends PropertiesLoader {
  val consumer = {
    val p = loadProperties("/consumer.properties")
    p("consumerKey") flatMap{ consumerKey  => p("consumerSecret") map{ consumerSecret =>
      Consumer( "http://twitter.com", consumerKey, consumerSecret )
  }}} getOrElse{ throw new RuntimeException("failed load consumer settings from /consumer.properties") }
  val accessToken = {
    val p = loadProperties("/accesstoken.properties")
    p("accessToken") flatMap{ token => p("tokenSecret") map { secret =>
      AccessToken( token, secret, consumer )
  }}} getOrElse{ throw new RuntimeException("failed load accesstoken settings from /accesstoken.properties") }

}
