package com.vacalendar

import cats.implicits._

import java.time.{ Instant, LocalDate }
import io.circe.Encoder
import io.circe.Decoder

package object endpoints {
  implicit val encodeInstant: Encoder[Instant] = Encoder.encodeString.contramap[Instant](_.toString)

  implicit val encodeLocalDate: Encoder[LocalDate] = Encoder.encodeString.contramap[LocalDate](_.toString)

  implicit val decodeLocalDate: Decoder[LocalDate] = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(LocalDate.parse(str)).leftMap(t => "LocalDate")
  }
}