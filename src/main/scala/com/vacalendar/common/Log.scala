package com.vacalendar.common

import cats.effect.Sync
import org.slf4j.LoggerFactory

trait Log[F[_]] {
  def info(value: String): F[Unit]
  def error(error: Throwable): F[Unit]
}

object Log {

  private val logger = LoggerFactory.getLogger(this.getClass)

  implicit def syncLogInstance[F[_]](implicit F: Sync[F]): Log[F] =
    new Log[F] {
      override def error(error: Throwable): F[Unit] = F.delay(logger.error(error.getMessage, error))
      override def info(value: String): F[Unit] = F.delay(logger.info(value))
    }
}
