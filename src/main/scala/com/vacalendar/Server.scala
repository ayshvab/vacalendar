package com.vacalendar

import cats.effect.{Effect, IO}
import com.vacalendar.conf.{DatabaseConfig, VacalendarConfig}
import com.vacalendar.domain.employees.{EmployeeService, EmployeeValidationInterpreter}
import com.vacalendar.endpoint.EmployeeEndpoints
import com.vacalendar.repository.{EmployeeRepoInterpreter, PositionRepoInterpreter}
import fs2.{Stream, StreamApp}
import fs2.StreamApp.ExitCode
import org.http4s.server.blaze.BlazeBuilder

object Server extends StreamApp[IO] {

  val apiV = "v1" // TODO move apiV parameter to Config

  override def stream(args: List[String], requestShutdown: IO[Unit]): fs2.Stream[IO, StreamApp.ExitCode] =
    createStream[IO](args, requestShutdown)

  def createStream[F[_]](args: List[String], requestShutdown: F[Unit])
                        (implicit E: Effect[F]): Stream[F, ExitCode] =
    for {
      conf <- Stream.eval(VacalendarConfig.load[F])
      xa <- Stream.eval(DatabaseConfig.dbTransactor(conf.db))
      _ <- Stream.eval(DatabaseConfig.initDb(conf.db, xa))

      positionRepo = PositionRepoInterpreter[F](xa)
      employeeRepo = EmployeeRepoInterpreter[F](xa)
      employeeValidation = EmployeeValidationInterpreter[F](employeeRepo, positionRepo)
      employeeService = EmployeeService[F](employeeRepo, employeeValidation)

      exitCode <- BlazeBuilder[F]
        .bindHttp(8080, "localhost")
        .mountService(EmployeeEndpoints.endpoints[F](employeeService), s"/$apiV/")
        .serve
    } yield exitCode
}

