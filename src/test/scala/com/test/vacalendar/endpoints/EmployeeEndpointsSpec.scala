package com.test.vacalendar.endpoints

import cats.effect._
import org.scalatest._
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}

import doobie.util.transactor.Transactor
import doobie._
import doobie.hikari.implicits._

import org.http4s._
import org.http4s.dsl._

import com.vacalendar.conf.DatabaseConfig
import com.vacalendar.Module
import com.vacalendar.validation.{ QryParamsValidationInterpreter, ServiceValidationInterpreter }
import com.vacalendar.repository.{ EmployeeRepoInterpreter, PositionRepoInterpreter }
import com.vacalendar.service.EmployeeService
import com.vacalendar.endpoints.{ EmployeeEndpoints, EndpointErrorHandler }

class EmployeeEndpointsSpec
  extends WordSpec
    with Matchers
    with ForAllTestContainer
    with Http4sDsl[IO] {

  import Http4sTestUtils._

  override val container = PostgreSQLContainer()

  def withCtx(testThunk: (Transactor[IO], HttpService[IO]) => IO[Unit]): Unit = {

    val dbConf = DatabaseConfig(driver = "org.postgresql.Driver",
                              url = container.jdbcUrl,
                              user = container.username,
                              password = container.password)

    val m = new Module[IO]()
    val xa = m.dbTransactor(dbConf).unsafeRunSync()

    val emplRepo = new EmployeeRepoInterpreter[IO](xa)
    val posRepo = new PositionRepoInterpreter[IO](xa)

    val serviceValidation = new ServiceValidationInterpreter()

    val emplService = new EmployeeService[IO](emplRepo, posRepo, serviceValidation)
    val errHandler = new EndpointErrorHandler[IO]()
    val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QryParamsValidationInterpreter, errHandler).service)

    m.migrateDb(xa).unsafeRunSync()

    try {
      testThunk(xa, emplEndpoints).unsafeRunSync()
    } finally {
      m.dropDb(xa).unsafeRunSync()
      xa.shutdown.unsafeRunSync()
    }
  }

  "Employee Endpoints" when {

    "GET /employees" should {
      "return empty array of employees" in withCtx {
        (xa, emplsEndpoints) => {

          val request = Request[IO](Method.GET, Uri.uri("/employees"))

          for {
            response <- emplsEndpoints
              .run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
          } yield {
            response.status shouldEqual Ok
            responseBody   shouldEqual "[]"
          }

        }
      }

    }

    // "GET /employees/:id" should {
    //   "Not Found when employee with id not exist" in withCtx {
    //     (xa, employeesEndpoints) => {
    //       val request = Request[IO](Method.GET, Uri.uri("/employees/1"))

    //       for {
    //         response <- employeesEndpoints
    //           .run(request)
    //           .getOrElse(fail(s"Request was not handled: $request"))
    //         responseBody <- response.as[String]
    //       } yield {
    //         response.status shouldEqual NotFound
    //       }

    //     }
    //   }
    // }


  }
}
