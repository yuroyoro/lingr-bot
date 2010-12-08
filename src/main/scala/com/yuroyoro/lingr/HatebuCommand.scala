package com.yuroyoro.lingr

object HatebuCommand extends Command with Crawler {
  val command:String = "hatebu"
  val usage:String = "<url> 指定エントリのブコメを表示する。"

  def apply(commandMessage:CommandMessage):Option[String] = {
    Some("そのうち実装するお")
  }
}
