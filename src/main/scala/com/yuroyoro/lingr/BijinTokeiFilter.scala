package com.yuroyoro.lingr

import java.net.{URL,HttpURLConnection}
import javax.servlet.{Filter,FilterChain, FilterConfig }
import javax.servlet.{ServletRequest, ServletResponse}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

class BijinTokeiFilter extends ImageFilter{

  val imageName:String = "bijin"

  override def doFilter(request:ServletRequest, response:ServletResponse, filter:FilterChain) = {
    val req = request.asInstanceOf[HttpServletRequest]
    val res = response.asInstanceOf[HttpServletResponse]

    val fname::area::info  = req.getRequestURI.split("/").reverse.toList
    val key = "%s_%s" format( area, fname)

    outputImage( key, res ){ () =>
      val url = "http://www.bijint.com/%s/tokei_images/%s".format( area, fname )
      val urlConn = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
      urlConn.addRequestProperty("REFERER","http://www.bijint.com/%s/" format area)
      urlConn.connect

      val in = urlConn.getInputStream
      Some(getImage(in))
    }
  }
}
