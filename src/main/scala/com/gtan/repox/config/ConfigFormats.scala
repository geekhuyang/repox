package com.gtan.repox.config

import com.gtan.repox.data.{Connector, ProxyServer, Repo}
import io.circe.Decoder.Result
import io.circe._, io.circe.generic.auto._, io.circe.parse._, io.circe.syntax._

import com.gtan.repox.data.DurationFormat._

case class ConnectorUsage(repo: Repo, connector: Connector)

case class ProxyUsage(connector: Connector, proxy: ProxyServer)

trait ConfigFormats {
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


  import ProxyServer._

  implicit val proxyUsageDecoder = new Decoder[Map[Connector, ProxyServer]] {
    override def apply(c: HCursor): Result[Map[Connector, ProxyServer]] =
      for (seq <- c.top.as[Seq[ProxyUsage]]) yield {
        seq.map(pu => pu.connector -> pu.proxy).toMap
      }
  }
  /*
        override def reads(json: JsValue): JsResult[Map[Connector, ProxyServer]] = try {
          (json: @unchecked) match {
            case JsArray(values) =>
              JsSuccess(values.map { value =>
                (value: @unchecked) match {
                  case obj: JsObject => (obj.fields: @unchecked) match {
                    case Seq(
                    ("connector", connectorJsVal: JsValue),
                    ("proxy", proxyJsVal: JsValue)) =>
                      connectorJsVal.as[Connector] -> proxyJsVal.as[ProxyServer]
                  }
                }
              } toMap)
          }
        } catch {
          case e: MatchError => JsError(s"Config.proxyUsage deserialize from json failed. $e")
        }
      }


    */


  implicit val proxyUsageEncoder = new Encoder[Map[Connector, ProxyServer]] {
    override def apply(a: Map[Connector, ProxyServer]): Json = a map {
      case (connector, proxy) => Json.obj("connector" -> connector.asJson, "proxy" -> proxy.asJson)
    } asJson
  }
}
