package com.yuroyoro.lingr

import scala.xml.XML
import  java.util.{Calendar, TimeZone}

object PingCommand extends Command with Crawler with TwitterApi {
  val command:String = "ping"
  val usage:String = "<twitter_id> twitter_idにmention投げて体育館裏に呼び出すお"

  def apply(commandMessage:CommandMessage):Option[String] = {
    commandMessage.args.headOption.flatMap{ twitterId =>
      val c = Calendar.getInstance
      c.setTimeZone(TimeZone.getTimeZone("GMT+9"))
      val time = "%04d/%02d/%02d %02d:%02d:%02d" format(
        c.get(Calendar.YEAR), c.get(Calendar.MONTH) + 1, c.get(Calendar.DAY_OF_MONTH),
        c.get(Calendar.HOUR_OF_DAY), c.get(Calendar.MINUTE), c.get(Calendar.SECOND))
      val msg = "@%s %sがLingrで呼んでるよ。%s http://lingr.com/room/%s" format( twitterId, commandMessage.nickname, time, commandMessage.room)

      val params = Map("status" -> msg )
      accessToken.post( "http://api.twitter.com/1/statuses/update.xml", params).flatMap{ src =>
        val xml = XML.loadString(src.mkString)
        (xml \\ "status").headOption.map{ status =>
          "%s\n%s\n%s http://twitter.com/#!/nyuryonyoro/status/%s" format( status \ "user" \ "profile_image_url" text, msg, status \ "created_at" text , status \ "id" text)
        }
      }
    }
  }
}

