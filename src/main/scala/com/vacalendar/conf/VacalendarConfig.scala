package com.vacalendar.conf

import cats.effect.Effect
import cats.implicits._
import pureconfig.error.ConfigReaderException

case class DatabaseConfig(driver: String, url: String, user: String, password: String)

case class VacalendarConfig(db: DatabaseConfig)

object VacalendarConfig {

  import pureconfig._

  def load[F[_]](implicit F: Effect[F]): F[VacalendarConfig] =
    F.delay(loadConfig[VacalendarConfig]("vacalendar")).flatMap {
      case Right(ok) => F.pure(ok)
      case Left(e) => F.raiseError(new ConfigReaderException[VacalendarConfig](e))
    }
}
