package com.yuroyoro.lingr
import scala.util.Random

object TWadaCommand extends Command {
  val command:String = "t_wada"
  val usage:String = "t_wada語録"

  val messages = List("""
1. java-ja なんだろ? 自重するなよ #keccon
http://farm5.static.flickr.com/4121/4760271503_b60476052d_m.jpg
2. java-ja から来ますた #keccon
http://farm5.static.flickr.com/4123/4758754123_810a39a4bd_m.jpg
3. java-ja に嗅ぎつけられた。終わった…
http://farm5.static.flickr.com/4093/4760156020_4e565c28f9_m.jpg
4. orz
http://farm5.static.flickr.com/4099/4758036252_67c20e2b3b_m.jpg
""",
"""
http://a2.twimg.com/profile_images/421959794/TQ_LOGO_normal.png
そういえば先週末は java-ja が東京にいない隙を突いてリアルライフをエンジョイさせていただきました。本当にありがとうございました。
http://twitter.com/#!/t_wada/statuses/10910517720
""",
"""
http://a2.twimg.com/profile_images/421959794/TQ_LOGO_normal.png
ポッキーゲームの日ですって？
http://twitter.com/#!/t_wada/status/5609284301
""",
"""
http://a2.twimg.com/profile_images/421959794/TQ_LOGO_normal.png
ススキノなう
http://twitter.com/#!/t_wada/status/6372709731
""",
"""
http://a2.twimg.com/profile_images/421959794/TQ_LOGO_normal.png
めるめるめるり
http://twitter.com/#!/t_wada/status/6756713220
""",
"""
http://a2.twimg.com/profile_images/421959794/TQ_LOGO_normal.png
ﾎﾃﾙﾆ ﾓﾄﾞｯﾃｷﾏｼﾀ｡ ｽｽｷﾉ ﾀﾉｼｶｯﾀﾃﾞｽ｡
http://twitter.com/#!/t_wada/status/10780444685500416
""",
"""
http://a2.twimg.com/profile_images/421959794/TQ_LOGO_normal.png
ナニを出せよ
http://twitter.com/#!/t_wada/status/17023308964
""",
"""
http://a2.twimg.com/profile_images/421959794/TQ_LOGO_normal.png
綺麗な踊り子さんと壇上で踊ってしまった<3
http://twitter.com/#!/t_wada/status/11086918180
"""
)

  def apply(commandMessage:CommandMessage):String = messages( Random.nextInt(messages.size))

}

