package com.gtan.repox.config

import com.gtan.repox.SerializationSupport
import com.gtan.repox.data.{Connector, ProxyServer, Repo}
import play.api.libs.json.{JsValue, Json}
import io.circe._, io.circe.generic.auto._, io.circe.parse._, io.circe.syntax._

object ProxyPersister extends SerializationSupport {

  case class NewOrUpdateProxy(proxy: ProxyServer) extends ConfigCmd {
    override def transform(old: Config) = {
      val oldProxies = old.proxies
      val oldProxyUsages: Map[Connector, ProxyServer] = old.proxyUsage
      old.copy(proxies = proxy.id.fold(oldProxies :+ proxy.copy(id = Some(ProxyServer.nextId.incrementAndGet()))) { _id =>
        oldProxies.map {
          case ProxyServer(Some(`_id`), _, _, _, _, _) => proxy
          case p => p
        }
      }, proxyUsage = proxy.id.fold(oldProxyUsages) { _id =>
        oldProxyUsages.map {
          case (connector, ProxyServer(Some(`_id`), _, _, _, _, _)) => connector -> proxy
          case u => u
        }
      })
    }
  }

  case class EnableProxy(id: Long) extends ConfigCmd {
    override def transform(old: Config) = {
      old.copy(proxies = old.proxies.map {
        case p@ProxyServer(Some(`id`), _, _, _, _, _) => p.copy(disabled = false)
        case p => p
      })
    }
  }

  case class DisableProxy(id: Long) extends ConfigCmd {
    override def transform(old: Config) = {
      old.copy(proxies = old.proxies.map {
        case p@ProxyServer(Some(`id`), _, _, _, _, _) => p.copy(disabled = true)
        case p => p
      }, proxyUsage = old.proxyUsage.filterNot {
        case (connector, proxy) => proxy.id.contains(id)
      })
    }
  }


  case class DeleteProxy(id: Long) extends ConfigCmd {
    override def transform(old: Config) = {
      old.copy(
        proxies = old.proxies.filterNot(_.id.contains(id)),
        proxyUsage = old.proxyUsage.filterNot { case (connector, proxy) => proxy.id.contains(id) }
      )
    }
  }

  val NewOrUpdateProxyClass = classOf[NewOrUpdateProxy].getName
  val EnableProxyClass = classOf[EnableProxy].getName
  val DisableProxyClass = classOf[DisableProxy].getName
  val DeleteProxyClass = classOf[DeleteProxy].getName

  override val reader: (Json) => PartialFunction[String, Jsonable] = payload => {
    case NewOrUpdateProxyClass => payload.as[NewOrUpdateProxy].toOption.get
    case DisableProxyClass => payload.as[DisableProxy].toOption.get
    case EnableProxyClass => payload.as[EnableProxy].toOption.get
    case DeleteProxyClass => payload.as[DeleteProxy].toOption.get
  }

  override val writer: PartialFunction[Jsonable, Json] = {
    case o: NewOrUpdateProxy => o.asJson
    case o: DisableProxy => o.asJson
    case o: EnableProxy => o.asJson
    case o: DeleteProxy => o.asJson
  }
}
