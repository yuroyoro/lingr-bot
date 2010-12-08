package com.yuroyoro.lingr

import java.net.{URL,HttpURLConnection}
import java.io._
import javax.servlet.{Filter,FilterChain, FilterConfig }
import javax.servlet.{ServletRequest, ServletResponse}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import scala.io.{Source, Codec}
class BijoKoyomiFilter extends Filter with Crawler{

  override def destroy = Unit
  override def init(config:FilterConfig) = Unit

  val r = """<img .*src="(/bijo/img2.php[^"]+)".*>""".r
  override def doFilter(request:ServletRequest, response:ServletResponse, filter:FilterChain) = {

    val req = request.asInstanceOf[HttpServletRequest]
    val res = response.asInstanceOf[HttpServletResponse]

    val path :: year :: month :: day :: info = req.getRequestURI.split("/").drop(1).toList
    val url = "http://www.bijogoyomi.com/bijo3/index.php/%s/%s/%s" format(year, month, day)
    crawl(url){html =>
      r.findFirstMatchIn(html).map{ _.group(1) }
    }.foreach{ imguri =>
      val urlConn = new URL("http://www.bijogoyomi.com" + imguri).openConnection.asInstanceOf[HttpURLConnection]
      urlConn.addRequestProperty("REFERER",url)
      urlConn.connect

      val in = urlConn.getInputStream
      val out = new ByteArrayOutputStream()
      val buf  =  Array.make( 1024 , (-1 ).toByte )

      def writeImage( len:Int ):Unit = {
        if( len > 0 ) {
          out.write( buf )
          writeImage( in.read( buf ) )
        }
      }
      out.flush
      out.close
      in.close

      writeImage( in.read( buf ) )

      res.setContentType("image/jpeg")
      res.setContentLength(out.size)
      out.writeTo(res.getOutputStream)
    }
  }
}
