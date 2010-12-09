package com.yuroyoro.lingr

object BrainFuckCommand extends Command {

  val command:String = "bf"
  val usage:String = "<src> <input> srcをbrainfuckのコードとして実行する"
  val bfParser = new BFParser(
    List("+"),
    List("-"),
    List(">"),
    List("<"),
    List("."),
    List(","),
    List("["),
    List("]"))

  def apply(commandMessage:CommandMessage):Option[String] = {
    commandMessage.args.headOption.flatMap{ src =>
      val in = commandMessage.args.toList.tail
      bfParser.parse(src).map{ op =>
        val bufRuntime = new BufferedBFRuntime( in.mkString(" "))
        bufRuntime.run(op, src)
        bufRuntime.result
      }
    }
  }
}
