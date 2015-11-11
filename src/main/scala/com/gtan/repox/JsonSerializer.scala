package com.gtan.repox

import java.io.NotSerializableException

import akka.serialization.Serializer
import com.gtan.repox.ExpirationManager.ExpirationSeq
import com.gtan.repox.config._
import com.typesafe.scalalogging.LazyLogging
import io.circe._, io.circe.generic.auto._, io.circe.parse._, io.circe.syntax._

trait SerializationSupport {
  val reader: Json => PartialFunction[String, Jsonable]
  val writer: PartialFunction[Jsonable, Json]
}

class JsonSerializer extends Serializer with LazyLogging with SerializationSupport {
  val ConfigChangedClass = classOf[ConfigChanged].getName

  val serializationSupports: Seq[_ <: SerializationSupport] = Seq(RepoPersister, ProxyPersister, ParameterPersister, Immediate404RulePersister, ExpireRulePersister, ConnectorPersister, ExpirationManager, ConfigPersister)

  override val reader: Json => PartialFunction[String, Jsonable] = { jsValue =>
    serializationSupports.map(_.reader(jsValue)).reduce(_ orElse _) orElse {
      case clazzName: String =>
        throw new NotSerializableException(s"No serialization supported for class $clazzName")
    }
  }
  override val writer: PartialFunction[Jsonable, Json] = serializationSupports.map(_.writer).reduce(_ orElse _) orElse {
    case jsonable: Jsonable =>
      throw new NotSerializableException(s"No serialization supported for $jsonable")
  }

  override def identifier: Int = 900188

  override def includeManifest: Boolean = false

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = manifest match {
    case None =>
      import cats.data.Xor._
      decode[Json](new String(bytes, "UTF-8")) match {
        case Right(json) =>
          json.asObject.map { obj =>
            (obj("manifest"), obj("config"), obj("cmd")) match {
              case (Some(mani), Some(config), Some(cmd)) if mani.isString =>
                ConfigChanged(configFromJson(config), jsonableFromJson(cmd))
              case _ =>
                throw new NotSerializableException(obj.toString)
            }
          }.orElse(json.asString filter (_ == "UseDefault") map (_ => UseDefault))
            .getOrElse(jsonableFromJson(json))

        case Left(error) =>
          throw new NotSerializableException(error.getMessage)
      }
    case Some(_) => throw new NotSerializableException("JsonSerializer does not use extra manifest.")
  }

  private def configFromJson(config: Json): Config =
    config.as[Config].fold(_ => throw new NotSerializableException(config.toString), identity)

  private def jsonableFromJson(evt: Json): Jsonable = evt.asObject match {
    case Some(obj) =>
      (obj("manifest"), obj("payload")) match {
        case (Some(mani), Some(payload)) if mani.isString =>
          reader.apply(payload).apply(mani.asString.get)
        case _ =>
          throw new NotSerializableException(evt.toString())
      }
    case _ => throw new NotSerializableException(evt.toString())
  }

  private def toJson(o: AnyRef): Json = o match {
    case ConfigChanged(config, jsonable) =>
      Json.obj(
        "manifest" -> classOf[ConfigChanged].getName.asJson,
        "config" -> config.asJson,
        "cmd" -> toJson(jsonable)
      )
    case jsonable: Jsonable =>
      Json.obj(
        "manifest" -> jsonable.getClass.getName.asJson,
        "payload" -> writer.apply(jsonable)
      )
    case UseDefault => "UseDefault".asJson
  }

  override def toBinary(o: AnyRef): Array[Byte] = toJson(o).toString().getBytes("UTF-8")
}
