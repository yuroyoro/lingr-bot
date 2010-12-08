package com.yuroyoro.lingr
import scala.xml.{XML, Node}

object WikiPediaCommand extends Command with XMLCrawler {

  val command:String = "wikipedia"
  val usage:String = "<args> argsの単語でWikiPedia検索"

  def apply(commandMessage:CommandMessage):Option[String] =  {
    def formatResult(xml:Node) = "%s %s\n\n%s\n" format( xml \ "title" text ,
      xml \ "url" text,
      (xml \ "body" text ).replaceAll("""<br/>""","\n"))


    Option(commandMessage.args.mkString(" ")).filter{ _.trim.nonEmpty }.flatMap{ word =>
      val url = "http://wikipedia.simpleapi.net/api?keyword=%s" format word.replaceAll(" ", "_")
      crawlXml(url){ xml =>
        val results = xml \\ "results" \ "result"
        results.filter { r => (r \ "strict").text.toInt == 1 }.headOption.map{
          formatResult
        }.orElse(
          Some(results.take(3).map{ formatResult }.mkString( "\n", ("-" * 40) + "\n", "\n"))
        )
      }
    }
  }

}

