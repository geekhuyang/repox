package com.gtan.repox

import java.time.Instant

import com.gtan.repox.data.ProxyServer
import com.gtan.repox.data.{ProxyServer, Connector, Repo}
import com.ning.http.client.{ProxyServer => JProxyServer}
import com.ning.http.client.ProxyServer.Protocol
import io.circe.Decoder.Result

import io.circe._, io.circe.generic.auto._, io.circe.parse._, io.circe.syntax._

import scala.concurrent.duration.Duration

case class ConnectorUsage(repo: Repo, connector: Connector)

case class ProxyUsage(connector: Connector, proxy: ProxyServer)

trait Codecs {

  implicit val DateTimeEncoder: Encoder[Instant] = new Encoder[Instant] {
    override def apply(a: Instant): Json = a.toEpochMilli.asJson
  }

  implicit val DateTimeDecoder: Decoder[Instant] = new Decoder[Instant] {
    override def apply(c: HCursor): Result[Instant] = c.top.as[Long].map(Instant.ofEpochMilli)
  }

  implicit val durationDecoder = new Decoder[Duration] {
    override def apply(c: HCursor): Result[Duration] = c.top.as[String].map(Duration.apply)
  }

  implicit val durationEncoder = new Encoder[Duration] {
    override def apply(a: Duration): Json = a.toString.asJson
  }

  implicit val protocolDecoder = new Decoder[JProxyServer.Protocol] {
    override def apply(c: HCursor): Result[Protocol] = c.top.as[String].map(JProxyServer.Protocol.valueOf)
  }

  implicit  val protocolEncoder = new Encoder[JProxyServer.Protocol] {
    override def apply(a: Protocol): Json = a.name.asJson
  }

  implicit val connectorUsageDecoder = new Decoder[Map[Repo, Connector]] {
    override def apply(c: HCursor): Result[Map[Repo, Connector]] =
      for (seq <- c.top.as[Seq[ConnectorUsage]]) yield {
        seq.map(cu => cu.repo -> cu.connector).toMap
      }

  }

  implicit val connectorUsageEncoder = new Encoder[Map[Repo, Connector]] {
    override def apply(a: Map[Repo, Connector]): Json = a map {
      case (repo, connector) => Json.obj("repo" -> repo.asJson, "connector" -> connector.asJson)
    } asJson
  }



  implicit val proxyUsageDecoder = new Decoder[Map[Connector, ProxyServer]] {
    override def apply(c: HCursor): Result[Map[Connector, ProxyServer]] =
      for (seq <- c.top.as[Seq[ProxyUsage]]) yield {
        seq.map(pu => pu.connector -> pu.proxy).toMap
      }
  }

  implicit val proxyUsageEncoder = new Encoder[Map[Connector, ProxyServer]] {
    override def apply(a: Map[Connector, ProxyServer]): Json = a map {
      case (connector, proxy) => Json.obj("connector" -> connector.asJson, "proxy" -> proxy.asJson)
    } asJson
  }

}
