package com.yuroyoro.lingr
/**
 * Brainf*ck派生言語のジェネレータ
 */

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position, NoPosition}

sealed abstract class Op {
  def exec( runtime:BFRuntime )
  val token:String
  val pos:Position
  override def toString = token
}

case class Inc( token:String, pos:Position ) extends Op {
  def exec( runtime:BFRuntime ) = runtime.inc
}
case class Dec( token:String, pos:Position ) extends Op{
  def exec( runtime:BFRuntime ) = runtime.dec
}
case class Next( token:String, pos:Position ) extends Op{
  def exec( runtime:BFRuntime ) = runtime.next
}
case class Prev( token:String, pos:Position ) extends Op{
  def exec( runtime:BFRuntime ) = runtime.prev
}
case class Get( token:String, pos:Position ) extends Op{
  def exec( runtime:BFRuntime ) = runtime.get
}
case class Put( token:String, pos:Position ) extends Op{
  def exec( runtime:BFRuntime ) = runtime.put
}
case class NoOp() extends Op{
  val token = ""
  val pos:Position = NoPosition
  def exec( runtime:BFRuntime ) = {}
}
case class Loop( op:List[Op], val pos:Position ) extends Op{
  val token = ""
  override def toString = "[" + op.mkString + "]"
  def exec( runtime:BFRuntime ) = {
    var cnt = 0
    while( runtime.m != 0 ){
      if( cnt > runtime.maxLoop )
        throw BFException("ループの実行回数が最大値を超えました。最大値:{%s}".format( runtime.maxLoop ))
      op.foreach( _.exec( runtime ) )
      cnt = cnt + 1
    }
  }
}

case class BFException(msg:String) extends java.lang.RuntimeException(msg)

trait BFRuntime {
  val size:Int
  val maxLoop:Int

  val offset = size / 2

  var mem = new Array[Int]( size )
  var pt = 0
  def p = pt + offset

  def inRange_? = if( pt < 0 || pt > size) throw BFException(
         "ポインタの位置がメモリの範囲を超えました。ポインタは-%s - %sの範囲にある必要があります。ポインタ位置:{%s}".format(offset, offset, p ))

  def m = {
    inRange_?
    mem(p)
  }
  def m( b:Int ) = {
    inRange_?
    mem(p) = b
  }

  def inc  = { m( m + 1 ) }
  def dec  = { m( m - 1 ) }
  def next = { pt = pt + 1 }
  def prev = { pt = pt - 1 }
  def get = m(in())
  def put = out( m.toChar )

  def in(): Int
  def out(c:Char):Unit

  def reset = {
    pt = 0
    mem = new Array[Int]( size )
  }

  def run(op:List[Op], source:String) = op.foreach{ e =>
    try{ e.exec( this ) }
    catch{ case BFException( msg ) => throw BFException("""line:%s column %s : %s
%s
%s""".format( e.pos.line, e.pos.column, msg,
        source.lines.drop( e.pos.line - 1 ).next,
        " " * ( e.pos.column ) + "^"))
    }
  }
}

class StandardBFRuntime( val size:Int = 30000, val maxLoop:Int = 65535 ) extends BFRuntime {
  def in(): Int = readChar.toInt
  def out(c:Char):Unit = print(c)

}

class BufferedBFRuntime( input:String,
  val size:Int = 30000, val maxLoop:Int = 65535 ) extends BFRuntime {

  import scala.collection.mutable.Queue
  val inQueue = Queue(input:_*)
  val outQueue = new Queue[Char]

  def in(): Int = inQueue.dequeue.toInt
  def out(c:Char):Unit = outQueue.enqueue(c)

  def result = outQueue.mkString
}


class BFParser(
  incTokens:List[String],
  decTokens:List[String],
  nextTokens:List[String],
  prevTokens:List[String],
  putTokens:List[String],
  getTokens:List[String],
  startTokens:List[String],
  endTokens:List[String]
)extends RegexParsers{
  import scala.util.parsing.input.CharSequenceReader._
  override def skipWhitespace = false

  def p(s:String):Parser[String] = s
  def make( tk:List[String] ) =
   wrap( ( p( tk.head ) /: tk.tail ){ _ ||| p( _ ) } )

  def wrap[A](p: Parser[A]) = Parser{r => Success(r.pos,  r)} ~ p

  def inc :Parser[Op] = ( make( incTokens ) ) ^^ { case ~(p,x) => Inc( x, p ) }
  def dec :Parser[Op] = ( make( decTokens ) ) ^^ { case ~(p,x) => Dec( x, p ) }
  def next:Parser[Op] = ( make( nextTokens ) ) ^^ { case ~(p,x) => Next( x, p ) }
  def prev:Parser[Op] = ( make( prevTokens ) ) ^^ { case ~(p,x) => Prev( x, p ) }
  def put :Parser[Op] = ( make( putTokens ) ) ^^ { case ~(p,x)  => Put( x, p ) }
  def get :Parser[Op] = ( make( getTokens ) ) ^^ { case ~(p,x) => Get( x, p ) }

  def start :Parser[Op] = ( make( startTokens ) ) ^^ ( x => NoOp() )
  def end   :Parser[Op] = ( make( endTokens ) ) ^^ ( x => NoOp() )
  val any   :Parser[Op] = elem("", _ != EofCh) ^^ ( x => NoOp() )

  def token       :Parser[Op] = inc  ||| dec ||| next |||
                                prev ||| get ||| put
  def comment     :Parser[Op] = not( token | start | end ) <~ any ^^ ( x => NoOp() )

  def loop        :Parser[Op] = wrap( start ~> rep(instruction) <~ end ) ^^ {
   case ~(p,op) => new Loop( op ,p )
  }

  def instruction :Parser[Op] = loop | token | comment
  def brainfuck   :Parser[List[Op]] = rep(instruction)

  def parse( s:String ):Option[List[Op]] = parseAll( brainfuck , s ) match {
    case Success( ops, _ )  =>  Some(ops)
    case Failure( msg, _ ) => { throw new BFException(msg) }
    case Error( msg, _ )   => { throw new BFException(msg) }
  }

  def genHelloWorld( sep:String ) = {
    wrapString(
      "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+.") map {
     case '+' => incTokens.head
     case '-' => decTokens.head
     case '>' => nextTokens.head
     case '<' => prevTokens.head
     case '.' => putTokens.head
     case ',' => getTokens.head
     case '[' => startTokens.head
     case ']' => endTokens.head
    } mkString( sep )

  }
}


///**
// * 各種サンプル
// */
//object Main {
//  def run( parser:BFParser, src:String) = parser.parse(src).foreach { op =>
//    println("=" * 80)
//    println("exec")
//    println(src)
//    println()
//    println("StandardBFRuntime")
//    val stdRuntime = new StandardBFRuntime
//    stdRuntime.run(op, src)
//    println()
//    println("BufferedBFRuntime")
//    val bufRuntime = new BufferedBFRuntime("")
//    bufRuntime.run(op, src)
//    println(bufRuntime.result)
//    println("=" * 80)
//  }
//  def main( args:Array[String] ) = {
//    val bfParser = new BFParser(
//      List("+"),
//      List("-"),
//      List(">"),
//      List("<"),
//      List("."),
//      List(","),
//      List("["),
//      List("]"))
//    // Hello World
//    println( "brainf*ck" )
//    run(bfParser, ">++++++ほげほげ+++[<+++++ほげほげ+++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>++++++ほげほげ+++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]+++++ほげほげ+++++.")
//    run(bfParser, ">++++++++++[<++++++++++>-]<++++.---.>++[<+++>-]<+..+++.>++++[>++++++++<-]>.<++++[<++>-]<.>++++[<-->-]<.+++.>++[<--->-]<.>++[<---->-]<.")
//    run(bfParser, "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+.")
//
//    // Error
//    try{ run(bfParser, ">+[>]") }catch{ case BFException(msg) => println( msg ) }
//    try{ run(bfParser, "<+[<]") }catch{ case BFException(msg) => println( msg ) }
//    try{ run(bfParser, ">+[]") }catch{ case BFException(msg) => println( msg ) }
//
//    // brainf*ckでジョジョ言語( http://d.hatena.ne.jp/toyoshi/20100208/1265587511 )
//    val jojoParser = new BFParser(
//      List("オラ"),
//      List("無駄"),
//      List("スターフィンガ" , "やれやれだぜ"),
//      List("ロードローラ" , "貧弱"),
//      List("ハーミットパープル"),
//      List("新手のスタンド使いか"),
//      List("あ・・・ありのまま今起こったことを話すぜ" ),
//      List("ザ・ワールド" ))
//
//    println()
//    println( "JoJo言語" )
//    run(jojoParser, """
//オラオラオラオラオラオラオラオラオラッ！！
//
//「あ・・・ありのまま今起こったことを話すぜ
//俺は奴の前で階段を登っていたと思ったら、いつの間にか降りていた
//な…何を言っているのかわからねーと思うが、
//俺も何をされたのかわからなかった…
//頭がどうにかなりそうだった…催眠術だとか超スピードだとか、
//そんなチャチなもんじゃあ断じてねえ。
//もっと恐ろしいものの片鱗を味わったぜ…」
//
//スターフィンガー！
//オラオララララ！
//オラッ！オラオラララララオラオラオラァ！！！
//スターフィンガー！！！
//オラァオラオラオラオラオラオラッオラ！！
//オラオラァァァァァオララララララララララ！
//スターフィンガー！
//
//オラオラオラオラオラ！　つけの領収書だぜ！
//
//力比べというわけか！
//知るがいい…！『ザ・ ワールド』の真の能力は…まさに！『世界を支配する』能力だと言うことを！
//
//「ロードローラだ！ロードローラだ！ロードローラだ！」
//無駄ッッッ！
//
//ザ・ワールドッッ
//
//スターフィンガー！
//「ハーミットパープル」
//スターフィンガー
//オラオラ！
//
//「ハーミットパープル」
//
//オラオラオラオラオラオラオラ
//ハーミットパープル！ハーミットパープル！
//
//オラオラオラ
//
//ハーミットパープル！
//スターフィンガー！
//
//無駄ァ！
//ハーミットパープル
//
//無駄！無駄！
//無駄無駄無駄無駄無駄無駄無駄無駄無駄無駄
//WRYYYYYYYYYYYYYY！
//“ジョースター・エジプト・ツアー御一行様”は貴様にとどめを刺して全滅の最後というわけだな
//
//ハーミットパープル！
//ロードローラだ！
//
//オーラオラオーラオラオラオラオーラオラオラオラオラッ！
//ハーミットパープル！
//無駄無駄無駄無駄無駄無駄無駄無駄ッ
//ハーミットパープル！
//オラオラオラアアアアアアアア！
//ハーミットパープル！
//無駄ッ無駄ッ無駄ッ無駄無駄無駄ァツ！
//
//ハーミットパープル
//
//もうおそい！　脱出不可能よッ！ 無駄無駄無駄無駄無駄無駄無駄無駄ぁぁ！
//ハーミットパープル！
//
//最高に『ハイ！』ってやつだアアアアア！アハハハハハハハハハーッ！！
//スターフィンガー
//オラ
//ハーミットパープル！
//
//てめーの敗因は・・・たったひとつだぜ・・・ＤＩＯ　たったひとつの単純（シンプル）な答えだ・・・　『てめーは　おれを怒らせた』
//""")
//
//    // プログラミング言語Misa(http://homepage2.nifty.com/kujira_niku/okayu/misa.html)
//    val misaParser = new BFParser(
//      List("+" , "あ" , "ぁ" , "お" , "ぉ" ),
//      List("-" , "っ" , "ッ") ,
//      List(">" , "→" , "～" , "ー"),
//      List("<" ,  "←" ,  "★" , "☆"),
//      List("." , "！" ),
//      List("," , "？" ),
//      List("[" , "「" , "『") ,
//      List("]" , "」" , "』"))
//
//    println()
//    println( "プログラミング言語Misa" )
//    run(misaParser, """
//ごっ、ごぉおっ、ご～きげんよおぉおおぉおほっ。ほおぉおぉおっ。
//
//「ごきげん☆みゃぁああ”あ”ぁ”ぁああ～っ」
//
//さわやかな朝の☆ご挨拶！　お挨拶がっ。
//澄みきった青空にこだましちゃうぉ～ああぉおおおぉん。
//
//「は、はひっ、はろおぉっ☆わぁるどおおぉっぉ～っ」
//
//こ、この文章は☆おサンプル！　おおぉおぉおおサンプルプログラム！！
//どんなおプログラム言語でも基本のご挨拶させていただくのぉぉおッ！
//
//「ぽうっ」
//
//長々と書くのがこ、ここでの～、ここでのぉおおぉおぉぉおたしなみぃぃいぃ。
//
//「長いぃ。長すぎましゅう。ご挨拶にこんなプログラム長すぎまひゅぅうぅ☆
//　んおおぉぉ、ばかになる、おばかになっちゃいましゅ～ッ」
//
//長いのがっ、バッファの奥まで入ってきましゅたぁあぁあっ！
//ばっふぁ☆溢れちゃいまひゅぅ～。あみゃぁあ”あ”ぁ”ぁああ”あ”ぁぁ。
//
//「で、出ます☆　んおおぉぉおおっ、エラー出ちゃいまひゅっ」
//
//ほひぃ☆！　え、えらーっ、んお”お”ぉお”お”ぉおぉおおぉっっ。
//
//「出た☆　出た出た出た出たぁぁあっ　えらあぴゅるーっって出たあぁっ」
//
//はしたない☆！　ぉおおぉはしたないっ！　おはしたない言語ですっっっっっっっ！
//おほっほおぉっっっほおぉっっっっっっっっっ！
//
//「えらあらいしゅきぃぃぃいぃっっ」
//
//止まらない　すごい　エラーみるく
//こってりしたのがいっぱい出てるよぉぉぉおおぉぉおおぉぉおっっ。
//
//「んほぉっ☆ っおぉぉぉおお国が分からなくなっちゃいまひゅう～っ」
//
//ま、まだ出るぅ☆　出てるのおぉっ☆　エラーまだまだ出ましゅぅぅ！
//ばんじゃ～ぁぁあい、ばんじゃいぃぃ、ばんにゃんじゃぁんじゃあぁぁああぁい！
//""")
//
//    // プログラミング言語「長門有希」(http://vipprog.net/wiki/プログラミング言語/Brainfuck.html#z8e76b58)
//    val yukiParser = new BFParser(
//      List("…"),
//      List("・"),
//      List("………。"),
//      List("…………。"),
//      List("………………。"),
//      List("……………。"),
//      List("「"),
//      List("」"))
//
//    println()
//    println( "プログラミング言語「長門有希」" )
//    println()
//    run(yukiParser,
//"""
//涼宮ハルヒのこと……… 。それと、わたしのこと……… 。あなたに教えておく…… 。
//
//（涼宮とお前が…何だって？）
//
//「うまく言語化できない………。情報の伝達に齟齬が発生するかもしれない………………
//…… 。でも、聞いて………。涼宮ハルヒとわたしは普通の人間じゃない……………… 。
//
//（なんとなく普通じゃないのは、わかるけどさ）
//
//そうじゃない…………… 。 性格に普遍的な性質を持っていないという意味ではなく、文
//字通りの意味で、彼女とわたしはあなたのような大多数の人間と同じとは言えない………。
//この銀河を統括する情報統合思念体によってつくられた対有機生命体コンタクト用ヒュー
//マノイドインターフェイス…………… 。それが、わたし…………。
//
//（はい？）
//
//通俗的な用語を使用すると宇宙人に該当する存在…………。
//
//（う、ちゅうじん？）
//
//わたしの仕事は涼宮ハルヒを観察して、入手した情報を統合思念体に報告すること…………。
//
//（えっ？）
//
//生み出されてから3年間、私はずっとそうやって過ごしてきた・・・。この3年間は特別な
//不確定要素がなく、いたって平穏。でも最近になって無視出来ないイレギュラー因子が涼
//宮ハルヒの周囲に現れた…… 。それが、あなた。」
//
//情報統合思念体にとって銀河の辺境に位置するこの星系の第３惑星に特別な価値などなか
//った………。でも現有生命体が地球と呼称するこの惑星で進化した二足歩行動物に知性と
//呼ばれる思索能力が芽生えたことにより、その重要度は増大した………………。もしかし
//たら自分たちが陥っている自律進化の閉塞状態を打開する可能性があるかも知れなかった
//から………。宇宙に偏在する有機生命体に意識が生じるのはありふれた現象だったが、高
//次の知性を持つまでに進化した例は地球人類が唯一だった。統合思念体は注意深くかつ綿
//密に観測を続けた…… 。
//
//そして3年前………………。惑星表面で他では類を見ない異常な情報フレアを観測した………………… 。
//弓状列島の一地域から噴出した情報爆発は瞬く間に惑星全土を覆い、惑星外空間に拡散し
//た………………。
//
//その中心にいたのが涼宮ハルヒ………………。
//以後3年間、あらゆる角度から涼宮ハルヒという個体に対し調査がなされた……… 。
//
//しかし未だその正体は不明………………。それでも統合思念体の一部は、彼女こそ人類の、
//ひいては情報生命体である自分たちに自律進化の切っ掛けを与える存在として涼宮ハルヒ
//の存在を解析を行っている………。情報生命体である彼らは有機生命体と直接的にコミュ
//ニケートできない・・・。言語を持たないから…… 。 人間は言葉を抜きにして概念を伝
//達する術を持たない………………。だからわたしのような人間用のインターフェイスを作
//った・・・・・・。情報統合思念体はわたしを通して人間とコンタクト出来る・・・・・・。
//
//涼宮ハルヒは自律進化の可能性を秘めている………………。恐らく彼女には自分の都合の
//良いように周辺の環境情報を操作する力がある…………。それが、わたしがここにいる理
//由。あなたがここにいる理由…………………… 。
//
//（待ってくれ。正直言おう、さっぱりわからない。）
//
//信じて………………。
//
//（そもそも、何で俺なんだ？いや、百歩譲ってお前の…その、情報なんとか体云々ってい
//うのを信用したとして、なぜ俺に正体を明かすんだ？）
//
//あなたは涼宮ハルヒに選ばれた・・・・・・・・・。涼宮ハルヒは意識的にしろ無意識的
//にしろ、自分の意思を絶対的な情報として環境に影響を及ぼす………………。あなたが選
//ばれたのには必ず理由がある……… 。
//
//（ねぇよ。）
//
//ある………………。あなたと涼宮ハルヒが、全ての可能性を握っている・・・・・・。
//
//（マジで言ってるのか？）
//
//もちろん………………。
//
//『度を越えた無口な奴が、やっと喋るようになったかと思ったら、永延電波な事を言いや
//がった。こんなトンデモ少女だったとは、さすがに想像外だぜ』
//
//（あのな、そんな話なら直でハルヒに言った方が喜ばれると思うぞ。はっきり言うが、俺
//はその手の話題には付いていけないんだ。悪いがな。）
//
//情報統合思念体の意識の大部分は、涼宮ハルヒが自分の存在価値と能力を自覚してしまう
//と予測できない危険を生む可能性があると認識している・・・。今はまだ様子を見るべき・・・・・。
//
//（俺が今聞いたこと、ハルヒに伝えるかもしれないじゃないか）
//
//彼女はあなたがもたらした情報を重視したりしない………………。
//
//『確かに。』
//
//情報統合思念体が地球に置いているインターフェイスは私一つではない………。情報統合
//思念体の意識の一部は積極的の動きを起こして情報の変動を観測しようとしている… 。あ
//なたは涼宮ハルヒにとっての鍵。危機が迫るとしたらまず、あなた………………。
//"""
//    )
//  }
//}
//
