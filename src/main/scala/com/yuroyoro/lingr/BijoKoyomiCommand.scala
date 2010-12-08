package com.yuroyoro.lingr

import  java.util.{Calendar, TimeZone}

object BijoKoyomiCommand extends Command with Crawler {
  val command:String = "bijo"
  val usage:String = "<yyyy/mm/dd> yyyy年mm月dd日の美女暦画像をだすの。引数省略すると現在日"

  def apply(commandMessage:CommandMessage):Option[String] =  {
    commandMessage.args.headOption.orElse{
      val c = Calendar.getInstance
      c.setTimeZone(TimeZone.getTimeZone("GMT+9"))
      Some("%04d/%02d/%02d".format( c.get(Calendar.YEAR), c.get(Calendar.MONTH) + 1, c.get(Calendar.DAY_OF_MONTH)))
    }.map{ t => "http://yuroyoro-lingr-bot.appspot.com/bijo/%s/bijo.jpg" format t }
  }

}
