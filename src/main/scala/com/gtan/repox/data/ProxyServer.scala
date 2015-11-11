package com.gtan.repox.data

import java.util.concurrent.atomic.AtomicLong

import com.gtan.repox.Repox
import com.gtan.repox.config.Config
import com.ning.http.client.ProxyServer.Protocol
import com.ning.http.client.{ProxyServer => JProxyServer}
import io.circe.Decoder.Result

import scala.collection.JavaConverters._
import io.circe._, io.circe.generic.auto._, io.circe.parse._, io.circe.syntax._

case class ProxyServer(id: Option[Long], name: String, protocol: JProxyServer.Protocol, host: String, port: Int, disabled: Boolean = false) {
  def toJava: JProxyServer = new JProxyServer(protocol, host, port)

}

object ProxyServer {
  lazy val nextId: AtomicLong = new AtomicLong(Config.proxies.flatMap(_.id).max)

  implicit val protocolDecoder = new Decoder[JProxyServer.Protocol] {
    override def apply(c: HCursor): Result[Protocol] = c.top.as[String].map(JProxyServer.Protocol.valueOf)
  }

  implicit  val protocolEncoder = new Encoder[JProxyServer.Protocol] {
    override def apply(a: Protocol): Json = a.name.asJson
  }
}
