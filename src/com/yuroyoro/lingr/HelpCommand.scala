package com.yuroyoro.lingr

object HelpCommand extends Command {
  val command:String = "help"
  val usage:String = "help表示するのん"

  def apply(commandMessage:CommandMessage):String = {
    val len = Bot.commands.map{ _.command.length }.max + 1
    val fmt = Bot.messagePrefix + "%-" + len + "s : %s"
    Bot.commands.map{ c => fmt format(c.command, c.usage) }.mkString("\n")
  }
}

