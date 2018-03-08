package com.vacalendar

import cats.effect.{Effect, IO}
import fs2.StreamApp.ExitCode
import fs2.{Scheduler, Stream, StreamApp}
import org.http4s.server.blaze.BlazeBuilder

import scala.concurrent.ExecutionContext.Implicits.global

import com.vacalendar.conf.VacalendarConfig


object Server extends HttpServer[IO]

class HttpServer[F[_]](implicit F: Effect[F]) extends StreamApp[F] {

  val apiV = "v1" 

  override def stream(args: List[String], requestShutdown: F[Unit]): Stream[F, ExitCode] = 
    Scheduler(corePoolSize = 2).flatMap { implicit scheduler =>
      for {
        conf <- Stream.eval(VacalendarConfig.load[F])

        ctx = new Module[F]()

        xa <- Stream.eval(ctx.dbTransactor(conf.db))

        _ <- Stream.eval(ctx.migrateDb(xa))

        exitCode <- BlazeBuilder[F]
          .bindHttp(sys.env.getOrElse("PORT", "8080").toInt, "0.0.0.0")
          .mountService(ctx.httpServices(xa), s"/$apiV/")
          .serve
        
      } yield exitCode
    }
}

