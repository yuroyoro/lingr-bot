package com.yuroyoro.lingr

import java.util.logging.Logger

import java.net.{URLEncoder, URLDecoder}
import javax.servlet.{Filter,FilterChain, FilterConfig }
import javax.servlet.{ServletRequest, ServletResponse}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import scala.io.Source
import scala.util.parsing.json.JSON

class LingrBotFilter extends Filter {
  override def destroy = Unit
  override def init(config:FilterConfig) = Unit

  def log(msg:String) = Logger.getLogger(classOf[LingrBotFilter].getName).info(msg)

  override def doFilter(request:ServletRequest, response:ServletResponse, filter:FilterChain) = {
    val req = request.asInstanceOf[HttpServletRequest]
    val res = response.asInstanceOf[HttpServletResponse]

    val body = Source.fromInputStream(req.getInputStream).mkString
    res.setContentType("text/plain")
    res.setCharacterEncoding("UTF-8")

    val testdata = URLEncoder.encode("""{"status":"ok", "counter":2320356, "events":[{"message":{"id":"1216064", "room":"yuroyoro_test", "public_session_id":"KpQ9lM", "icon_url":"http://www.gravatar.com/avatar/bd3590aaffe8948079d27795cb6f7388.jpg", "type":"user", "speaker_id":"yuroyoro", "nickname":"ゆろよろ", "text":"&ping yuroyoro ", "timestamp":"2010-12-07T10:35:58Z", "local_id":"pending-KpQ9lM-13"}, "event_id":2320356}]}""", "utf-8")

    body.split("json=").lastOption.filter{ _.trim.nonEmpty }.orElse(Option(req.getParameter("json"))).orElse(Some(testdata)).map{ s =>
    // body.split("json=").lastOption.filter{ _.trim.nonEmpty }.map{ s =>
      URLDecoder.decode(s, "utf-8")
    }.foreach{ s =>
      JSON.parse(s).foreach{ json => json.collect {
        case ("events", xs) => {
          val messages = xs.asInstanceOf[List[List[(String, List[(String, String)])]]]
          messages.map{ _.head }.collect{
            case ("message", l) => l.toMap
          }.flatMap{ m =>
            log("trace : " + m )
            Bot(Message(m("nickname"), m("text"), m("room")))
          }.mkString("\n")
        }
      }.foreach { result =>
        log("result: " + result )
        res.getWriter.println(result)
      }}
    }
  }
}
