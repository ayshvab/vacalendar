package com.vacalendar.conf

import cats.effect.Effect
import cats.implicits._
import pureconfig.error.ConfigReaderException

case class VacalendarConfig(db: DatabaseConfig)

object VacalendarConfig {

  import pureconfig._

  def load[F[_]](implicit E: Effect[F]): F[VacalendarConfig] =
    E.delay(loadConfig[VacalendarConfig]("vacalendar")).flatMap {
      case Right(ok) => E.pure(ok)
      case Left(e) => E.raiseError(new ConfigReaderException[VacalendarConfig](e))
    }
}
