package com.yuroyoro.lingr

import scala.util.Random

object DioCommand extends RandomFunctionCommand {
  val command:String = "dio"
  val usage:String = "貧弱！貧弱ゥ！"

  def choice[T](xs:T *):T = xs( Random.nextInt(xs.size) )

  implicit val max = 20
  def rnd(implicit n:Int = max ) = Random.nextInt(n) + 1
  def gen(s:String, n:Int = max) = s * rnd(n)

  val workers:Seq[CommandMessage => String] = Seq(
    { cm => "「%s%s%s%s%s」" format(
        gen("無駄"), gen("ァ", 3), gen("―", 5),
        gen("ッ", 3), gen("！", 5))},
    { cm => "「%s貧弱%s%s」" format(
        gen("貧弱！", 3), gen("ゥ", 3), gen("！", 5))},
    { cm => "「%sＲ%s%s」" format(
        choice("Ｗ", "Ｕ"), gen("Ｙ"), gen("！", 5))},
    { cm => "「ロードローラーだッ！」" },
    { cm => "「最高に『ハイ！』ってやつだアアアアア！アハハハハハハハハハーッ！！」" },
    { cm => "「おまえは今まで食ったパンの枚数を覚えているのか？」" },
    { cm => "「おれは人間をやめるぞ！ジョジョーーーーッ！！」" },
    { cm => "「%s！きさま！見ているなッ！」" format(cm.nickname) },
    { cm => "「『世界（ザ･ワールド）』　時よ止まれッ！」" }
  )

  val functions:Seq[CommandMessage => Option[String]] = workers.map{ f => { cm:CommandMessage => Some("%s\n%s" format(
    "http://img.f.hatena.ne.jp/images/fotolife/y/yuroyoro/20101209/20101209134338.png?1291876531?changed=1291876531", f(cm)))}}.toSeq
}

