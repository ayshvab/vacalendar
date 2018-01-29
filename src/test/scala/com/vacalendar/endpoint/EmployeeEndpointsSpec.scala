package com.vacalendar.endpoint

import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import doobie.util.transactor.Transactor
import org.http4s._
import org.http4s.dsl._
import org.scalatest._
import cats.effect.IO
import com.vacalendar.conf.DatabaseConfig
import com.vacalendar.domain.employees.{EmployeeService, EmployeeValidationInterpreter}
import com.vacalendar.repository.{EmployeeRepoInterpreter, PositionRepoInterpreter}

class EmployeesEndpointsSpec
  extends WordSpec
    with Matchers
    with ForAllTestContainer
    with Http4sDsl[IO] {

  override val container = PostgreSQLContainer()

  def withCtx(testThunk: (Transactor[IO], HttpService[IO]) => IO[Unit]): Unit = {

    val dbConf = DatabaseConfig(driver = "org.postgresql.Driver",
                                  url = container.jdbcUrl,
                                  user = container.username,
                                  password = container.password)

    val xa = DatabaseConfig.dbTransactor[IO](dbConf).unsafeRunSync()

    DatabaseConfig.initDb(dbConf, xa).unsafeRunSync()

    val positionRepo = PositionRepoInterpreter[IO](xa)
    val employeeRepo = EmployeeRepoInterpreter[IO](xa)
    val employeeValidation = EmployeeValidationInterpreter[IO](employeeRepo, positionRepo)
    val employeeService = EmployeeService[IO](employeeRepo, employeeValidation)
    val employeeEndpoints = EmployeeEndpoints.endpoints[IO](employeeService)

    try {
      testThunk(xa, employeeEndpoints).unsafeRunSync()
    } finally {
      DatabaseConfig.dropDb(dbConf, xa).unsafeRunSync()
    }
  }

  "Employees" when {

    "GET /employees" should {
      "return empty array of employees" in withCtx {
        (xa, employeesEndpoints) => {

          val request = Request[IO](Method.GET, Uri.uri("/employees"))

          for {
            response <- employeesEndpoints
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

    "GET /employees/:id" should {
      "Not Found when employee with id not exist" in withCtx {
        (xa, employeesEndpoints) => {
          val request = Request[IO](Method.GET, Uri.uri("/employees/1"))

          for {
            response <- employeesEndpoints
              .run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
          } yield {
            response.status shouldEqual NotFound
          }

        }
      }

      

    }
  }
}
