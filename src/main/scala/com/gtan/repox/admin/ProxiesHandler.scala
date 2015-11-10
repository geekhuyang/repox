package com.gtan.repox.admin

import java.net.URLDecoder

import com.gtan.repox.Repox
import com.gtan.repox.config.Config
import com.gtan.repox.config.ProxyPersister._
import com.gtan.repox.data.ProxyServer
import io.undertow.server.HttpServerExchange
import io.undertow.util.{HttpString, Methods}
import collection.JavaConverters._
import akka.pattern.ask
import concurrent.duration._
import io.circe._, io.circe.generic.auto._, io.circe.parse._, io.circe.syntax._

object ProxiesHandler extends RestHandler {
  implicit val timeout = akka.util.Timeout(5 seconds)

  import WebConfigHandler._

  override def route(implicit exchange: HttpServerExchange): PartialFunction[(HttpString, String), Unit] = {
    case (Methods.GET, "proxies") =>
      respondJson(exchange, Config.proxies)
    case (Methods.POST, "proxy") | (Methods.PUT, "proxy") =>
      val newV = exchange.getQueryParameters.get("v").getFirst
      val proxy = decode[ProxyServer](newV).toOption.get
      setConfigAndRespond(exchange, Repox.configPersister ? NewOrUpdateProxy(proxy))
    case (Methods.PUT, "proxy/enable") =>
      val newV = exchange.getQueryParameters.get("v").getFirst
      setConfigAndRespond(exchange, Repox.configPersister ? EnableProxy(newV.toLong))
    case (Methods.PUT, "proxy/disable") =>
      val newV = exchange.getQueryParameters.get("v").getFirst
      setConfigAndRespond(exchange, Repox.configPersister ? DisableProxy(newV.toLong))
    case (Methods.DELETE, "proxy") =>
      val newV = exchange.getQueryParameters.get("v").getFirst
      setConfigAndRespond(exchange, Repox.configPersister ? DeleteProxy(newV.toLong))
  }
}
