package com.yuroyoro.lingr

object VoteCommand extends Command {
  val command:String = "vote"
  val usage:String = "<key> <create|result|yes|no|> <issuename> 投票する。'vote sample create サンプル投票'で議題作成。'vote sample yes'で賛成。noで反対。'vote sample result'で集計結果表示。"

  def apply(commandMessage:CommandMessage):Option[String] = {
    Some("そのうち実装するお")
  }

}
