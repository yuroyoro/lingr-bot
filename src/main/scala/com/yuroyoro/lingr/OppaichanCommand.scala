package com.yuroyoro.lingr

object OppaichanCommand extends Command {

  val command:String = "oppaichan"
  val usage:String = "おっぱい！おぱーい！"

  def apply(commandMessage:CommandMessage):Option[String] =
    Some("http://yuroyoro-lingr-bot.appspot.com/images/oppaichan.jpg")
}

