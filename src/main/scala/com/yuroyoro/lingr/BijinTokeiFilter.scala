package com.yuroyoro.lingr

import java.net.{URL,HttpURLConnection}
import java.io._
import javax.servlet.{Filter,FilterChain, FilterConfig }
import javax.servlet.{ServletRequest, ServletResponse}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

class BijinTokeiFilter extends Filter {

  override def destroy = Unit
  override def init(config:FilterConfig) = Unit

  override def doFilter(request:ServletRequest, response:ServletResponse, filter:FilterChain) = {
    val req = request.asInstanceOf[HttpServletRequest]
    val res = response.asInstanceOf[HttpServletResponse]

    val fname = req.getRequestURI.split("/").last
    val url = "http://bijint.com/jp/img/clk/%s".format( fname )

    val urlConn = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
    urlConn.addRequestProperty("REFERER","http://bijint.com/jp/")
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
