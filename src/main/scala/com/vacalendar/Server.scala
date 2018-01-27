package com.vacalendar

import cats.effect.{Effect, IO}
import com.vacalendar.conf.{DatabaseConfig, VacalendarConfig}
import com.vacalendar.domain.employees.EmployeeService
import com.vacalendar.endpoint.EmployeeEndpoints
import com.vacalendar.repository.EmployeeRepoInterpreter
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

      employeeRepo = EmployeeRepoInterpreter[F](xa)
      employeeService = EmployeeService[F](employeeRepo)

      exitCode <- BlazeBuilder[F]
        .bindHttp(8080, "localhost")
        .mountService(EmployeeEndpoints.endpoints[F](employeeService), s"/$apiV/")
        .serve
    } yield exitCode
}

//  // ==> Errors <==
//  class ErrorCode(val value: String) extends AnyVal // or Enumeration of ErrorCodes
//
//  trait Error {
//    val code: ErrorCode
//    val message: String
////    val target
////    val details
////    val innererror
//  }
//
//  sealed abstract class ServiceError extends Error
//  sealed abstract class DatabaseError extends Error
//  sealed abstract class RequestError extends Error
//
//  sealed abstract class ApiError
//  object ApiError {
////    final case class Database(err: DatabaseErr) extends ApiError
//    final case class Service(err: ServiceError) extends ApiError
////    final case class Request(err: RequestErr) extends ApiError
//  }


//  // ==== Responses ====
//  case class ErrorResponse(error: Err)
//  case class ApiErrorResp(error: Error)
//  // ==== *** ====
