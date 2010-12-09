package com.yuroyoro.lingr

import  java.util.{Calendar, TimeZone}

object BijinTokeiCommand extends Command {
  val areas = Seq(
    ("jp", "美人時計"),
    ("binan", "美男時計"),
    ("hokkaido", "北海道"),
    ("sendai", "仙台"),
    ("kobe", "神戸"),
    ("kyoto", "京都"),
    ("kanazawa", "金沢"),
    ("fukuoka", "福岡"),
    ("okayama", "岡山"),
    ("kagawa", "香川"),
    ("nagoya", "名古屋"),
    ("kagoshima", "鹿児島"),
    ("niigata", "新潟"),
    ("kr", "韓国"),
    ("cc", "サーキット2010"),
    ("k-musume", "カンバン娘"),
    ("k-danshi", "カンバン男子"),
    ("bimajo", "美魔女時計"),
    ("kids", "キッズ時計"))

  val command:String = "bijin"
  val usage:String = "<area> <HHmm> HH時mm分の美人時計画像をだすの。引数省略すると現在時刻。サポートするareaは" + areas.map{ _.toString }.mkString(", ")


  val Area = areas.map{ case (a, _) => a }.mkString("(", "|", ")$").r
  val Time = """(\d{4})$""".r

  def now = {
    val c = Calendar.getInstance
    c.setTimeZone(TimeZone.getTimeZone("GMT+9"))
    "%02d%02d".format( c.get(Calendar.HOUR_OF_DAY), c.get(Calendar.MINUTE))
  }

  def apply(commandMessage:CommandMessage):Option[String] =  {
    val (area, time ) = commandMessage.args match {
      case Nil => ("jp", now)
      case Area(area):: Nil => (area, now)
      case Time(time):: Nil => ("jp", time)
      case Area(area):: Time(time) :: Nil => (area, time)
      case _ => ("jp", now)
    }
    Some("http://yuroyoro-lingr-bot.appspot.com/bijin/%s/%s.jpg" format(area, time)  )
  }

}
