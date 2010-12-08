package com.yuroyoro.lingr

object DictCommand extends Command with Crawler {
  val command:String = "dict"
  val usage:String = "<word> 辞書を引く"

  def apply(commandMessage:CommandMessage):Option[String] = {
    Some("そのうち実装するお")
  }
}

