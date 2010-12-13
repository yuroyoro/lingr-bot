package com.yuroyoro.lingr

import java.net.{URL,HttpURLConnection}
import javax.servlet.{Filter,FilterChain, FilterConfig }
import javax.servlet.{ServletRequest, ServletResponse}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

class BijoKoyomiFilter extends ImageFilter with Crawler{

  val imageName:String = "bijo"

  val r = """<img .*src="(/bijo/img2.php[^"]+)".*>""".r
  override def doFilter(request:ServletRequest, response:ServletResponse, filter:FilterChain) = {

    val req = request.asInstanceOf[HttpServletRequest]
    val res = response.asInstanceOf[HttpServletResponse]

    val path :: year :: month :: day :: info = req.getRequestURI.split("/").drop(1).toList
    val key = "%s_%s_%s" format( year, month, day )

    outputImage( key, res ){ () =>
      val url = "http://www.bijogoyomi.com/bijo3/index.php/%s/%s/%s" format(year, month, day)
      crawl(url){html =>
        r.findFirstMatchIn(html).map{ _.group(1) }
      }.map{ imguri =>
        val urlConn = new URL("http://www.bijogoyomi.com" + imguri).openConnection.asInstanceOf[HttpURLConnection]
        urlConn.addRequestProperty("REFERER",url)
        urlConn.connect
        val in = urlConn.getInputStream
        getImage(in)
      }
    }
  }
}
