package com.gtan.repox.admin

import java.util.Date

import com.gtan.repox.Repox
import com.gtan.repox.config.ConfigPersister.SaveSnapshot
import com.gtan.repox.config.{ParameterPersister, ConfigPersister, Config}
import com.typesafe.scalalogging.LazyLogging
import io.undertow.server.HttpServerExchange
import io.undertow.server.handlers.{CookieImpl, Cookie}
import io.undertow.util.{Cookies, StatusCodes, Methods}
import akka.pattern.ask
import concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}
import concurrent.ExecutionContext.Implicits.global
import io.circe._, io.circe.generic.auto._, io.circe.parse._, io.circe.syntax._

object AuthHandler extends RestHandler with LazyLogging {

  import WebConfigHandler._
  import ParameterPersister._

  implicit val timeout = akka.util.Timeout(1 second)


  override def route(implicit exchange: HttpServerExchange) = {
    case (Methods.POST, "login") =>
      val pass = exchange.getQueryParameters.get("v").getFirst
      exchange.setStatusCode(StatusCodes.OK)
      if (Config.password == pass) {
        exchange.setResponseCookie(new CookieImpl("authenticated", "true").setPath("/admin"))
        exchange.getResponseSender.send( """{"success": true}""")
      } else {
        exchange.getResponseSender.send( """{"success": false}""")
      }
    case (Methods.POST, "logout") =>
      exchange.setStatusCode(StatusCodes.OK)
      exchange.setResponseCookie(new CookieImpl("authenticated", "true").setPath("/admin").setMaxAge(0))
      exchange.getRequestCookies.remove("authenticated")
      exchange.getResponseChannel
      exchange.endExchange()
    case (Methods.POST, "saveSnapshot") =>
      (Repox.configPersister ? SaveSnapshot).onComplete { result =>
        exchange.getResponseSender.send( s"""{"success": ${result.isSuccess}}""")
      }
    case (Methods.PUT, "password") =>
      val v = exchange.getQueryParameters.get("v").getFirst
      decode[Map[String, String]](v).toOption.fold[Unit](exchange.setStatusCode(StatusCodes.BAD_REQUEST).endExchange()) { map =>
        (map.get("p1"), map.get("p2")) match {
          case (Some(p1), Some(p2)) =>
            if (p1 == p2) {
              setConfigAndRespond(exchange, Repox.configPersister ? ModifyPassword(p1))
            } else {
              exchange.setStatusCode(StatusCodes.BAD_REQUEST).endExchange()
            }
          case _ =>
            exchange.setStatusCode(StatusCodes.BAD_REQUEST).endExchange()
        }
      }
  }
}
