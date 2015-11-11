package com.gtan.repox.admin

import java.net.URLDecoder

import akka.pattern.ask
import com.gtan.repox.{Codecs, Repox}
import com.gtan.repox.config.{Config, ConfigPersister}
import com.gtan.repox.data.Connector
import io.circe.Json
import io.undertow.server.HttpServerExchange
import io.undertow.util.{HttpString, Methods}

import scala.concurrent.duration._
import scala.language.postfixOps
import io.circe._, io.circe.generic.auto._, io.circe.parse._, io.circe.syntax._

object ConnectorsHandler extends RestHandler with Codecs{

  import com.gtan.repox.admin.WebConfigHandler._
  import com.gtan.repox.config.ConnectorPersister._

  implicit val timeout = akka.util.Timeout(1 second)

  override def route(implicit exchange: HttpServerExchange): PartialFunction[(HttpString, String), Unit] = {
    case (Methods.GET, "connectors") =>
      val config = Config.get
      respondJson(exchange, Json.obj(
        "connectors" -> config.connectors.map(ConnectorVO.wrap).asJson,
        "proxies" -> config.proxies.asJson)
      )
    case (Methods.POST, "connector") =>
      val newV = exchange.getQueryParameters.get("v").getFirst
      val vo = decode[ConnectorVO](newV).toOption.get
      setConfigAndRespond(exchange, Repox.configPersister ? NewConnector(vo))
    case (Methods.PUT, "connector") =>
      val newV = exchange.getQueryParameters.get("v").getFirst
      val vo = decode[ConnectorVO](newV).toOption.get
      setConfigAndRespond(exchange, Repox.configPersister ? UpdateConnector(vo))
    case (Methods.DELETE, "connector") =>
      val newV = exchange.getQueryParameters.get("v").getFirst
      setConfigAndRespond(exchange, Repox.configPersister ? DeleteConnector(newV.toLong))

  }
}
