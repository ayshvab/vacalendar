package com.vacalendar.conf

import cats.effect.{Async, Sync}
import doobie.hikari.HikariTransactor
import org.flywaydb.core.Flyway

case class DatabaseConfig(driver: String, url: String, user: String, password: String)

object DatabaseConfig {

  def dbTransactor[F[_]: Async](dbConfig: DatabaseConfig): F[HikariTransactor[F]] =
    HikariTransactor.newHikariTransactor[F](dbConfig.driver, dbConfig.url, dbConfig.user, dbConfig.password)


  def initDb[F[_]](dbConfig: DatabaseConfig, xa: HikariTransactor[F])
                  (implicit S: Sync[F]): F[Unit] = xa.configure { ds =>
      S.delay {
        val fw = new Flyway()
        fw.setDataSource(ds)
        fw.migrate()
        ()
      }
    }

  def dropDb[F[_]](dbConfig: DatabaseConfig, xa: HikariTransactor[F])
                  (implicit S: Sync[F]): F[Unit] = xa.configure { ds =>
    S.delay {
      val fw = new Flyway()
      fw.setDataSource(ds)
      fw.clean()
      ()
    }
  }

}
