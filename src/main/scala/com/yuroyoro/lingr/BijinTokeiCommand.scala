package com.yuroyoro.lingr

import  java.util.{Calendar, TimeZone}

object BijinTokeiCommand extends Command {
  val command:String = "bijin"
  val usage:String = "<HHmm> HH時mm分の美人時計画像をだすの。引数省略すると現在時刻"

  def apply(commandMessage:CommandMessage):Option[String] =  {
    commandMessage.args.headOption.orElse{
      val c = Calendar.getInstance
      c.setTimeZone(TimeZone.getTimeZone("GMT+9"))
      Some("%02d%02d".format( c.get(Calendar.HOUR_OF_DAY), c.get(Calendar.MINUTE)))
    }.map{ t => "http://yuroyoro-lingr-bot.appspot.com/bijin/%s.jpg" format t }
  }

}
