package com.yuroyoro.lingr


import java.net.{URL,HttpURLConnection}
import java.io._
import java.util.logging.Logger
import javax.servlet.{Filter,FilterChain, FilterConfig }
import javax.servlet.{ServletRequest, ServletResponse}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import com.yuroyoro.util.gae._
import com.google.appengine.api.datastore._

trait ImageFilter extends Filter {

  override def destroy = Unit
  override def init(config:FilterConfig) = Unit

  def log(msg:String) = Logger.getLogger(classOf[ImageFilter].getName).info(msg)
  val imageName:String

  def outputImage( key:String, res:HttpServletResponse)( provideImageFunction:() => Option[Array[Byte]]) = {
    val cacheKey = "&s_%s" format(imageName, key)

    Base64Cache(cacheKey).map{b =>
      log ("Cache Hit %s %s" format( imageName, key))
      b
    }.orElse({
      val dsKey = DataStore.createKey(imageName, key)
      DataStore.get(dsKey).map{ e =>
        log("DataStore Hit %s %s" format(imageName, key))
        e.getProperty("image").asInstanceOf[Blob].getBytes
      }
    }).orElse({
      log("DataStore no hit %s %s" format(imageName, key))
      provideImageFunction().map{ bytes =>
        val e = DataStore.createEntity(imageName, key)
        e.setProperty("image", new Blob( bytes))
        DataStore.put( e )

        Base64Cache(cacheKey) = bytes
        bytes
      }
    }).foreach { bytes =>
      res.setContentType("image/jpeg")
      res.setContentLength(bytes.size)
      res.getOutputStream.write( bytes )
      res.getOutputStream.flush
      res.getOutputStream.close
    }
  }

  def getImage(in:InputStream):Array[Byte] = {
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
    out.toByteArray
  }
}
