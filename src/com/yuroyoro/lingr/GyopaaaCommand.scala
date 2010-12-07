package com.yuroyoro.lingr

import scala.util.Random

class GyopaaaMarkov( sentences:Seq[String] ) {

  sealed abstract class Token
  case object Head extends Token
  case object Tail extends Token
  case class Word( c:Char ) extends Token

  case class Node( token:Token, next:Seq[Token] ){
    def choice = next( Random.nextInt( next.size ))
  }

  val sp = "?!。、！？…"
  val em = "・.,?!。、！？‥…"
  val emr = (".*[" + em + "]$").r

  val tokens = sentences.flatMap{ s =>
    val tokens =  Head :: s.map{ c => Word( c ) }.toList ::: Tail :: Nil
    tokens.sliding(2).toList.collect { case Seq(c1, c2) => (c1, c2) }
  }.distinct
  val dict = tokens.groupBy{ case( t, n ) => t }.map{ case (t ,s) =>
    Node( t, s.map{ case( _, n ) => n } )}.toList

  def choiceNode( word:Token ):Node = {
    val nodes = dict.collect{ case n @ Node(`word`, _) => n }
    nodes( Random.nextInt( nodes.size ) )
  }

  def generate = {
    def chain( n:Node ):List[Char]= n.choice match {
      case t:Word=> t.c :: chain( choiceNode( t ) )
      case _ => Nil
    }
    val rv = chain( choiceNode( Head ) ).mkString
    rv match{
      case emr() => rv
      case _ => rv + sp( Random.nextInt( sp.size  ))
    }
  }
}

object GyopaaaCommand extends Command {
  val seq = Seq(
     "ふぬいっ", "ふぬるぷ" , "ふにょー", "ふぬわーっ",
     "ぎょぱー", "ぎゃっぱぎゃっぱ", "ぬふふ", "ほにゅわーっ",
     "ぎゃっぱぎゃっぱ", "へぺっ", "もるぁー", "もるすぁー",
     "ひゃっはーーーっ", "きゃっきゃうふふ", "ひゃぴーっ", "へぬぇ",
     "きぇぇぇ", "はにゃー", "へぷぇ", "ぬるぽっ", "わふーっ", "あぃっ",
     "おんどるぁ", "あひぃ", "ひぎぃ", "らめぇー", "おるぁ", "ごるぁ",
     "おぺぺぺぺ", "ごぶぁ", "らぬぃぱぁ", "あっぁぁあああ",
     "ぅぅうううわぁああああん！！！", "っぁっ………",
     "あふぅ……んっ", "ぃぃいやぁん", "ふぇぁあっ",
     "あぁああああ…ああ…あっあっー！", "あぁああああああ！！！",
     "あぁぁ…くんくん",
     "んはぁっ！", "モフモフ！", "…きゅんきゅんきゅい！！",
     "よぅ！！", "あぁぁああ…あああ…あっあぁああ！", "ふぁぁああんんっ！！",
     "いやぁああ！！", "にゃああん！！", "ぎゃあああ！！",
     "いやっほぉおお！！！", "ううっうぅうう！"
  )
  val markov = new GyopaaaMarkov( seq )
  val command:String = "gyopaa"
  val usage:String = "<length> 奇声を発する。lengthで文章の長さ指定(max 10)。"

  def apply(commandMessage:CommandMessage):String = {
    commandMessage.args.headOption.flatMap{ cnt =>
      import scala.util.control.Exception._
      allCatch.opt{ cnt.toInt }
    }.orElse(Some(3)).map{ cnt =>
      ( 1 to Seq(10, cnt).min).map{ n =>
         ( 1 to ( 1 + Random.nextInt(3))).map{ m => markov.generate }.mkString
       }.mkString("\n")
    }.getOrElse("")
  }
}

