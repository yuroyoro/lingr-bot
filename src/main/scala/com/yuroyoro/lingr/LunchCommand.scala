package com.yuroyoro.lingr

object LunchCommand extends Command with Crawler {
  val command:String = "lunch"
  val usage:String = "昼飯を提案するのです。"

  def apply(commandMessage:CommandMessage):Option[String] = {
    Some("そのうち実装するお")
  }
}

// vim: set ts=4 sw=4 et:
