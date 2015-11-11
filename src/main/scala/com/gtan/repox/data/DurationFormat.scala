package com.gtan.repox.data

import scala.concurrent.duration.Duration
import io.circe._, io.circe.generic.auto._, io.circe.parse._, io.circe.syntax._
import Decoder.Result

object DurationFormat {
  implicit val durationDecoder = new Decoder[Duration] {
    override def apply(c: HCursor): Result[Duration] = c.top.as[String].map(Duration.apply)
  }

  implicit val durationEncoder = new Encoder[Duration] {
    override def apply(a: Duration): Json = a.toString.asJson
  }

}
