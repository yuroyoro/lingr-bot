package com.yuroyoro.lingr
import java.util.logging.Logger

case class Message(nickname:String, text:String, room:String)
case class CommandMessage(nickname:String, text:String, room:String, command:String, args:Seq[String])

object Bot {

  val messagePrefix = '&'
  val commands:Seq[Command]= Seq(
    HelpCommand,
    MisawaCommand,
    GyopaaaCommand,
    MorphologicalAnalyserCommand,
    TWadaCommand
  )

  def apply(message:Message):Seq[String] = {
    message.text.lines.toList.filter{ s =>
      s.startsWith( messagePrefix.toString )
    }.flatMap{ s =>
      val cs::args = s.split("\\s").toList
      val cmd = cs.dropWhile(messagePrefix==)
      val commandMessage = CommandMessage( message.nickname,
        message.text, message.room, cmd, args)
      commands.filter{ c => c.matchCommand_?(commandMessage) }.map{ c => c(commandMessage)}
    }
  }
}

trait Command {
  def log(msg:String) = Logger.getLogger(classOf[LingrBotFilter].getName).info(msg)

  val command:String
  val usage:String
  def apply(commandMessage:CommandMessage):String
  def matchCommand_?(commandMessage:CommandMessage) =
    commandMessage.command == command
}

