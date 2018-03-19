package com.test.vacalendar.endpoints

import java.time._
import cats.effect._
import org.scalatest._
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}

import doobie.hikari.implicits._

import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._

import org.http4s._
import org.http4s.dsl.Http4sDsl

import com.vacalendar.conf.DatabaseConfig
import com.vacalendar.Module
import com.vacalendar.validation.{ QryParamsValidationInterpreter => QV, ServiceValidationInterpreter => SV }
import com.vacalendar.repository.{ EmployeeRepoInterpreter, VacationRepoInterpreter}
import com.vacalendar.service.VacationService
import com.vacalendar.endpoints._
import com.vacalendar.endpoints.QryParams._
import com.vacalendar.domain._

class EmployeeVacationsEndpointsSpec
  extends WordSpec
    with Matchers
    with ForAllTestContainer
    with Http4sDsl[IO] {

  import Http4sTestUtils._

  implicit val vacInDecoder = jsonOf[IO, VacationIn]

  override val container = PostgreSQLContainer()

  def withCtx(testThunk: (EmployeeRepoInterpreter[IO],
                          VacationRepoInterpreter[IO]) => IO[Assertion]): Unit = {

    val dbConf = DatabaseConfig(driver = "org.postgresql.Driver",
                                url = container.jdbcUrl,
                                user = container.username,
                                password = container.password)

    val m = new Module[IO]()
    val xa = m.dbTransactor(dbConf).unsafeRunSync()

    val emplRepo = new EmployeeRepoInterpreter[IO](xa)
    val vacRepo = new VacationRepoInterpreter[IO](xa)

    m.migrateDb(xa).unsafeRunSync()

    try {
      testThunk(emplRepo, vacRepo).unsafeRunSync()
    } finally {
      m.dropDb(xa).unsafeRunSync()
      xa.shutdown.unsafeRunSync()
    }
  }

  "Create vacation" when {

    "valid vacation input" should {
      "create" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clock).service)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val vacIn = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value
            
            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations")
            request <- Request[IO](Method.POST, uri).withBody(vacIn.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "vacationId" : 1,
              |  "employeeId" : 1,
              |  "since" : "2018-07-01",
              |  "until" : "2018-07-06",
              |  "created" : "2018-05-01T00:00:00Z",
              |  "updated" : null
              |}""".stripMargin

              created <- vacRepo.getVac(empl1.employeeId, 1).value.map(_.right.get)
          } yield {
            response.status shouldEqual Created
            responseBody shouldEqual expectedBody
            created.asJson.spaces2 shouldEqual expectedBody
          }
        }
      }
    }

    "employee with employeeId not exist" should {
      "return error Employee Not Found" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clock).service)

          val nonExistingEmplId: Long = 1
          val vacIn = VacationIn(since = LocalDate.of(2018, 3, 1), until = LocalDate.of(2018, 3, 10))
          
          val uri = Uri(path = s"/employees/${nonExistingEmplId}/vacations")

          for {
            request <- Request[IO](Method.POST, uri).withBody(vacIn.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "EmplNotFound",
              |    "message" : "Employee not found"
              |  }
              |}""".stripMargin
          } yield {
            response.status shouldEqual NotFound
            responseBody shouldEqual expectedBody
          }
        }
      }
    }

    "vacation input has invalid data" should {
      "fail on basic validation" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clock).service)

          val emplIn = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val vacIn1 = VacationIn(since = LocalDate.of(2018, 1, 20), until = LocalDate.of(2017, 12, 25))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 20), until = LocalDate.of(2018, 7, 21))

          for {
            empl <- emplRepo.createEmpl(emplIn).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl.employeeId}/vacations")
            request1 <- Request[IO](Method.POST, uri).withBody(vacIn1.asJson)
            response1 <- vacEndpoints.run(request1)
              .getOrElse(fail(s"Request was not handled: $request1"))
            responseBody1 <- response1.as[String]

            request2 <- Request[IO](Method.POST, uri).withBody(vacIn2.asJson)
            response2 <- vacEndpoints.run(request2)
              .getOrElse(fail(s"Request was not handled: $request2"))
            responseBody2 <- response2.as[String]

            expectedBody1 =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "VacSinceDateMustBeBeforeUntilDate",
              |        "message" : "Since date must be before until date"
              |      },
              |      {
              |        "code" : "VacOnlyInFuture",
              |        "message" : "New or updated vacation must be only in future"
              |      },
              |      {
              |        "code" : "VacMustStartAndEndWithin1Year",
              |        "message" : "Vacation must start and end within one year"
              |      },
              |      {
              |        "code" : "VacPeriodIsMoreMax",
              |        "message" : "Vacation period is more than the maximum"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            expectedBody2 =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "VacPeriodIsLessMin",
              |        "message" : "Vacation period is less than the minimum"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            createdVacs <- vacRepo.getVacs(empl.employeeId, VacsQryParams()).value
          } yield {
            createdVacs shouldEqual Right(List())

            response1.status shouldEqual BadRequest
            responseBody1 shouldEqual expectedBody1

            response2.status shouldEqual BadRequest
            responseBody2 shouldEqual expectedBody2
          }
        }
      }

      "fail on too many employees of one position will on vac in the same time" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clock).service)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val emplIn3 = EmployeeIn(firstName = "Frank", lastName = "Cassady", positionId = 1)
          val emplIn4 = EmployeeIn(firstName = "Jim", lastName = "Burton", positionId = 1)
          val emplIn5 = EmployeeIn(firstName = "Malcolm", lastName = "Montgomery", positionId = 1)
          val emplIn6 = EmployeeIn(firstName = "Walter", lastName = "White", positionId = 2)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 20), until = LocalDate.of(2018, 7, 25))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 15), until = LocalDate.of(2018, 7, 22))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 9, 5), until = LocalDate.of(2018, 9, 15))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            empl2 <- emplRepo.createEmpl(emplIn2).value.map(_.right.get)
            empl3 <- emplRepo.createEmpl(emplIn3).value.map(_.right.get)

            empl4 <- emplRepo.createEmpl(emplIn4).value
            empl5 <- emplRepo.createEmpl(emplIn5).value
            empl6 <- emplRepo.createEmpl(emplIn6).value.map(_.right.get)

            _ <- vacService.createVac(empl1.employeeId, vacIn1, clock).value
            _ <- vacService.createVac(empl2.employeeId, vacIn2, clock).value
            vac3 <- vacService.createVac(empl3.employeeId, vacIn3, clock).value.map(_.right.get)
            _ <- vacService.createVac(empl6.employeeId, vacIn3, clock).value

            uri = Uri(path = s"/employees/${empl3.employeeId}/vacations")
            request <- Request[IO](Method.POST, uri).withBody(vacIn1.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "TooManyEmplsOfOnePosOnVac",
              |        "message" : "Too many employees of one position will be on vacation at the same time"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- vacRepo.getVacs(empl3.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac3))
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
          }

        }
      }

      "fail on max count vacation days per year exceeded" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clock).service)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val vacIn1 = VacationIn(since = LocalDate.of(2018, 6, 15), until = LocalDate.of(2018, 6, 30))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 8, 15), until = LocalDate.of(2018, 8, 30))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value
            vac1 <- vacService.createVac(empl1.employeeId, vacIn1, clock).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations")
            request <- Request[IO](Method.POST, uri).withBody(vacIn2.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "MaxCountVacDaysPerYearExceeded",
              |        "message" : "The maximum count of vacation days per year is exceeded"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- vacRepo.getVacs(empl1.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac1))
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
          }
        }
      }

      "fail on not enough days from new vacation to next vacation" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clock).service)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 6, 20), until = LocalDate.of(2018, 6, 30))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1, clock).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations")
            request <- Request[IO](Method.POST, uri).withBody(vacIn2.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "NotEnoughDaysToNextVac",
              |        "message" : "The minimum period between vacation periods is equal to the size of the first vacation"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- vacRepo.getVacs(empl1.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac1))
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
          }
        }
      }

      "fail on validation creation: not enough days pass from prev vacation to new vacation" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clock).service)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 11))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 15), until = LocalDate.of(2018, 7, 26))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1, clock).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations")
            request <- Request[IO](Method.POST, uri).withBody(vacIn2.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "NotEnoughDaysPassFromLastVac",
              |        "message" : "The minimum period between vacation periods is equal to the size of the first vacation"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- vacRepo.getVacs(empl1.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac1))
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
          }
        }
      }

      "fail on validation creation: not enough days between prev vacation/new vacation and between new vacation/next vacation" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clock).service)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 20), until = LocalDate.of(2018, 7, 25))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 7, 9), until = LocalDate.of(2018, 7, 16))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1, clock).value.map(_.right.get)
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2, clock).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations")
            request <- Request[IO](Method.POST, uri).withBody(vacIn3.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "NotEnoughDaysPassFromLastVac",
              |        "message" : "The minimum period between vacation periods is equal to the size of the first vacation"
              |      },
              |      {
              |        "code" : "NotEnoughDaysToNextVac",
              |        "message" : "The minimum period between vacation periods is equal to the size of the first vacation"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- vacRepo.getVacs(empl1.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac1, vac2))
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
          }
        }
      }

      "fail on validation creation: vacations should not overlapped" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clock).service)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 10))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 8), until = LocalDate.of(2018, 7, 15))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1, clock).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations")
            request <- Request[IO](Method.POST, uri).withBody(vacIn2.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "VacsMustNotOverlap",
              |        "message" : "Employee vacations must not overlap with each other"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- vacRepo.getVacs(empl1.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac1))
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
          }
        }
      }

    }

  }

  "Update vacation" when {
    "valid vacation input" should {
      "update" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val clockUpdate = Clock.fixed(LocalDate.of(2018, 6, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpointsUpdMoment = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockUpdate).service)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 7, 21), until = LocalDate.of(2018, 7, 27))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 8, 8), until = LocalDate.of(2018, 8, 20))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacService.createVac(empl1.employeeId, vacIn1, clockCreate).value
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2, clockCreate).value.map(_.right.get)
            _ <- vacService.createVac(empl1.employeeId, vacIn3, clockCreate).value

            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations/${vac2.vacationId}")
            request <- Request[IO](Method.PATCH, uri).withBody(updVacIn.asJson)
            response <- vacEndpointsUpdMoment.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "vacationId" : 2,
              |  "employeeId" : 1,
              |  "since" : "2018-08-08",
              |  "until" : "2018-08-20",
              |  "created" : "2018-05-01T00:00:00Z",
              |  "updated" : "2018-06-01T00:00:00Z"
              |}""".stripMargin
          } yield {
            response.status shouldEqual Ok
            responseBody shouldEqual expectedBody
          }
        }
      }
    }

    "employee or vacations not found" should {

      "fail on employee not found" in withCtx {
        (emplRepo, vacRepo) => {
          val clockUpdate = Clock.fixed(LocalDate.of(2018, 6, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpointsUpdMoment = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockUpdate).service)

          val updVacIn = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 10))

          val nonExistingEmplId = 1
          val nonExistingVacId = 1
          val uri = Uri(path = s"/employees/${nonExistingEmplId}/vacations/${nonExistingVacId}")

          for {
            request <- Request[IO](Method.PATCH, uri).withBody(updVacIn.asJson)
            response <- vacEndpointsUpdMoment.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "EmplNotFound",
              |    "message" : "Employee not found"
              |  }
              |}""".stripMargin
          } yield {
            response.status shouldEqual NotFound
            responseBody shouldEqual expectedBody
          }
        }
      }

      "fail on vacation not found" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val clockUpdate = Clock.fixed(LocalDate.of(2018, 6, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpointsUpdMoment = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockUpdate).service)
          
          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val vacIn = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 10))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 9, 5), until = LocalDate.of(2018, 9, 12))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac <- vacService.createVac(empl.employeeId, vacIn, clockCreate).value.map(_.right.get)

            nonExistingVacId = 2
            uri = Uri(path = s"/employees/${empl.employeeId}/vacations/${nonExistingVacId}")
            request <- Request[IO](Method.PATCH, uri).withBody(updVacIn.asJson)
            response <- vacEndpointsUpdMoment.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "VacNotFound",
              |    "message" : "Vacation not found"
              |  }
              |}""".stripMargin

            created <- vacRepo.getVacs(empl.employeeId, VacsQryParams()).value
          } yield {
            response.status shouldEqual NotFound
            responseBody shouldEqual expectedBody
            created shouldEqual Right(List(vac))            
          }
        }
      }  
    }

    "update past or current vacation" should {

      "fail on only the upcoming vacation can be deleted or modified" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val clockUpdate = Clock.fixed(LocalDate.of(2018, 6, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpointsUpdMoment = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockUpdate).service)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn = VacationIn(since = LocalDate.of(2018, 3, 1), until = LocalDate.of(2018, 3, 10))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 9, 5), until = LocalDate.of(2018, 9, 12))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac <- vacRepo.createVac(empl.employeeId, vacIn, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl.employeeId}/vacations/${vac.vacationId}")
            request <- Request[IO](Method.PATCH, uri).withBody(updVacIn.asJson)
            response <- vacEndpointsUpdMoment.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "CannotChangeOrDeleteNotFutureVac",
              |    "message" : "Only the upcoming vacation can be deleted or modified"
              |  }
              |}""".stripMargin


            created <- vacRepo.getVac(empl.employeeId, vac.vacationId).value
          } yield {
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
            created shouldEqual Right(Some(vac))
          }
        }
      }
    }

    "not valid vacation input" should {
      "fail on too many employees of one position will on vac in the same time" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val emplIn3 = EmployeeIn(firstName = "Frank", lastName = "Cassady", positionId = 1)
          val emplIn4 = EmployeeIn(firstName = "Jim", lastName = "Burton", positionId = 1)
          val emplIn5 = EmployeeIn(firstName = "Malcolm", lastName = "Montgomery", positionId = 1)
          val emplIn6 = EmployeeIn(firstName = "Walter", lastName = "White", positionId = 2)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 20), until = LocalDate.of(2018, 7, 25))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 15), until = LocalDate.of(2018, 7, 22))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 9, 5), until = LocalDate.of(2018, 9, 15))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            empl2 <- emplRepo.createEmpl(emplIn2).value.map(_.right.get)
            empl3 <- emplRepo.createEmpl(emplIn3).value.map(_.right.get)

            empl4 <- emplRepo.createEmpl(emplIn4).value
            empl5 <- emplRepo.createEmpl(emplIn5).value
            empl6 <- emplRepo.createEmpl(emplIn6).value.map(_.right.get)

            _ <- vacService.createVac(empl1.employeeId, vacIn1, clockCreate).value
            _ <- vacService.createVac(empl2.employeeId, vacIn2, clockCreate).value
            vac3 <- vacService.createVac(empl3.employeeId, vacIn3, clockCreate).value.map(_.right.get)
            _ <- vacService.createVac(empl6.employeeId, vacIn3, clockCreate).value

            uri = Uri(path = s"/employees/${empl3.employeeId}/vacations/${vac3.vacationId}")
            request <- Request[IO](Method.PATCH, uri).withBody(vacIn1.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "TooManyEmplsOfOnePosOnVac",
              |        "message" : "Too many employees of one position will be on vacation at the same time"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- vacRepo.getVacs(empl3.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac3))
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
          }
        }
      }

      "fail on not enough days between prev vacation & new vacation and between new vacation & next vacation" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)          

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 7, 21), until = LocalDate.of(2018, 7, 27))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 7, 8), until = LocalDate.of(2018, 7, 20))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacService.createVac(empl1.employeeId, vacIn1, clockCreate).value.map(_.right.get)
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2, clockCreate).value.map(_.right.get)
            _ <- vacService.createVac(empl1.employeeId, vacIn3, clockCreate).value.map(_.right.get)
            
            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations/${vac2.vacationId}")
            request <- Request[IO](Method.PATCH, uri).withBody(updVacIn.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "NotEnoughDaysPassFromLastVac",
              |        "message" : "The minimum period between vacation periods is equal to the size of the first vacation"
              |      },
              |      {
              |        "code" : "NotEnoughDaysToNextVac",
              |        "message" : "The minimum period between vacation periods is equal to the size of the first vacation"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- vacRepo.getVac(empl1.employeeId, vac2.vacationId).value
          } yield {
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
            created shouldEqual Right(Some(vac2))
          }
        }
      }

      "fail on vacations should not overlapped" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)          

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 10))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 20), until = LocalDate.of(2018, 7, 25))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 7, 8), until = LocalDate.of(2018, 7, 15))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacService.createVac(empl1.employeeId, vacIn1, clockCreate).value
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations/${vac2.vacationId}")
            request <- Request[IO](Method.PATCH, uri).withBody(updVacIn.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "VacsMustNotOverlap",
              |        "message" : "Employee vacations must not overlap with each other"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- vacRepo.getVac(empl1.employeeId, vac2.vacationId).value
          } yield {
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
            created shouldEqual Right(Some(vac2))
          }
        }
      }

      "fail on max count vacation days per year exceeded" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)          

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 16))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 8, 1), until = LocalDate.of(2018, 8, 6))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 8, 1), until = LocalDate.of(2018, 8, 16))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacService.createVac(empl1.employeeId, vacIn1, clockCreate).value
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2, clockCreate).value.map(_.right.get)
            
            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations/${vac2.vacationId}")
            request <- Request[IO](Method.PATCH, uri).withBody(updVacIn.asJson)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "MaxCountVacDaysPerYearExceeded",
              |        "message" : "The maximum count of vacation days per year is exceeded"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- vacRepo.getVac(empl1.employeeId, vac2.vacationId).value
          } yield {
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
            created shouldEqual Right(Some(vac2))
          }

        }
      }
    }
  }

  "Delete vacation" when {
    "vacation not found" should {
      "return error: vacation not found" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)          

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val vacIn = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 10))
          
          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value
            _ <- vacService.createVac(empl.employeeId, vacIn, clockCreate).value.map(_.right.get)
            
            nonExistingVacId = 2
            uri = Uri(path = s"/employees/${empl.employeeId}/vacations/$nonExistingVacId")
            request = Request[IO](Method.DELETE, uri)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "VacNotFound",
              |    "message" : "Vacation not found"
              |  }
              |}""".stripMargin

          } yield {
            response.status shouldEqual NotFound
            responseBody shouldEqual expectedBody
          }
        }
      }
    }

    "delete past or current vacation" should {
      "return error" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)          

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val vacIn = VacationIn(since = LocalDate.of(2018, 3, 1), until = LocalDate.of(2018, 3, 10))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value
            vac <- vacRepo.createVac(empl.employeeId, vacIn, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl.employeeId}/vacations/${vac.vacationId}")
            request = Request[IO](Method.DELETE, uri)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "CannotChangeOrDeleteNotFutureVac",
              |    "message" : "Only the upcoming vacation can be deleted or modified"
              |  }
              |}""".stripMargin

            optFound <- vacRepo.getVac(empl.employeeId, vac.vacationId).value
          } yield {
            optFound shouldEqual Right(Some(vac))
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
          }

        }
      }
    }

    "delete future vacation" should {

      "delete" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)          

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn = VacationIn(since = LocalDate.of(2018, 6, 1), until = LocalDate.of(2018, 6, 10))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value
            vac <- vacRepo.createVac(empl.employeeId, vacIn, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl.employeeId}/vacations/${vac.vacationId}")
            request = Request[IO](Method.DELETE, uri)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
          
            optFound <- vacRepo.getVac(empl.employeeId, vac.vacationId).value
          } yield {
            optFound shouldEqual Right(None)
            response.status shouldEqual NoContent
          }
        }
      }
    }
  }

  "Get vacation" when {
    "Vacation exist" should {
      "return vacation" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)          

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val vacIn = VacationIn(since = LocalDate.of(2018, 6, 1), until = LocalDate.of(2018, 6, 10))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac <- vacRepo.createVac(empl.employeeId, vacIn, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl.employeeId}/vacations/${vac.vacationId}")
            request = Request[IO](Method.GET, uri)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "vacationId" : 1,
              |  "employeeId" : 1,
              |  "since" : "2018-06-01",
              |  "until" : "2018-06-10",
              |  "created" : "2018-05-01T00:00:00Z",
              |  "updated" : null
              |}""".stripMargin

          } yield {
            response.status shouldEqual Ok
            responseBody shouldEqual expectedBody
          }
        }
      }
    }

    "Vacation not exist" should {
      "return No Found" in withCtx {
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)          

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val vacIn = VacationIn(since = LocalDate.of(2018, 6, 1), until = LocalDate.of(2018, 6, 10))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value
            _ <- vacRepo.createVac(empl.employeeId, vacIn).value
            
            nonExistingVacId = 2
            uri = Uri(path = s"/employees/${empl.employeeId}/vacations/$nonExistingVacId")
            request = Request[IO](Method.GET, uri)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody = ""
          } yield {
            response.status shouldEqual NotFound
            responseBody shouldEqual expectedBody
          }
        }
      }
    }
  }

  "Get vacations by query params" when {
    "employee not exist" should {
      "return error" in withCtx { 
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)
          
          val notExistingEmplId = 1
          val uri = Uri(path = s"/employees/$notExistingEmplId/vacations")
          val request = Request[IO](Method.GET, uri)
          for {
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "EmplNotFound",
              |    "message" : "Employee not found"
              |  }
              |}""".stripMargin
          } yield {
            response.status shouldEqual NotFound
            responseBody shouldEqual expectedBody
          }
        }
      }
    }

    "employee without vacations" should {
      "return empty list of vacations" in withCtx { 
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)

          val emplIn = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)

          for {
            empl <- emplRepo.createEmpl(emplIn).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl.employeeId}/vacations")
            request = Request[IO](Method.GET, uri)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """[
              |]""".stripMargin
          } yield {
            response.status shouldEqual Ok
            responseBody shouldEqual expectedBody
          }
        }
      }
    }

    "with query params and employee with vacations" should {
      "return list of vacations in asc order by vacation_id" in withCtx { 
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 7, 21), until = LocalDate.of(2018, 7, 27))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl.employeeId, vacIn1, clockCreate).value.map(_.right.get)
            vac2 <- vacService.createVac(empl.employeeId, vacIn2, clockCreate).value.map(_.right.get)
            vac3 <- vacService.createVac(empl.employeeId, vacIn3, clockCreate).value.map(_.right.get)
            
            uri = Uri(path = s"/employees/${empl.employeeId}/vacations")
            request = Request[IO](Method.GET, uri)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """[
              |  {
              |    "vacationId" : 1,
              |    "employeeId" : 1,
              |    "since" : "2018-07-01",
              |    "until" : "2018-07-06",
              |    "created" : "2018-05-01T00:00:00Z",
              |    "updated" : null
              |  },
              |  {
              |    "vacationId" : 2,
              |    "employeeId" : 1,
              |    "since" : "2018-07-11",
              |    "until" : "2018-07-16",
              |    "created" : "2018-05-01T00:00:00Z",
              |    "updated" : null
              |  },
              |  {
              |    "vacationId" : 3,
              |    "employeeId" : 1,
              |    "since" : "2018-07-21",
              |    "until" : "2018-07-27",
              |    "created" : "2018-05-01T00:00:00Z",
              |    "updated" : null
              |  }
              |]""".stripMargin
          } yield {
            response.status shouldEqual Ok
            responseBody shouldEqual expectedBody
          }
        }
      }

      "return filtered list of vacations in asc order by since date" in withCtx { 
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 8, 21), until = LocalDate.of(2018, 8, 27))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1, clockCreate).value.map(_.right.get)
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2, clockCreate).value.map(_.right.get)
            _ <- vacService.createVac(empl1.employeeId, vacIn3, clockCreate).value

            
            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations", 
                      query = Query(("orderBy", Some("since")), ("since", Some("_..2018-08-21"))))
            request = Request[IO](Method.GET, uri)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """[
              |  {
              |    "vacationId" : 1,
              |    "employeeId" : 1,
              |    "since" : "2018-07-01",
              |    "until" : "2018-07-06",
              |    "created" : "2018-05-01T00:00:00Z",
              |    "updated" : null
              |  },
              |  {
              |    "vacationId" : 2,
              |    "employeeId" : 1,
              |    "since" : "2018-07-11",
              |    "until" : "2018-07-16",
              |    "created" : "2018-05-01T00:00:00Z",
              |    "updated" : null
              |  }
              |]""".stripMargin
          } yield {
            response.status shouldEqual Ok
            responseBody shouldEqual expectedBody
          }
          
        }
      }

      "return filtered list" in withCtx { 
        (emplRepo, vacRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val vacEndpoints = ioMiddleware(new EmployeeVacationsEndpoints[IO](vacService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 8, 21), until = LocalDate.of(2018, 8, 27))
          
          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacService.createVac(empl1.employeeId, vacIn1, clockCreate).value
            _ <- vacService.createVac(empl1.employeeId, vacIn2, clockCreate).value
            vac3 <- vacService.createVac(empl1.employeeId, vacIn3, clockCreate).value.map(_.right.get)
            
            uri = Uri(path = s"/employees/${empl1.employeeId}/vacations", 
                      query = Query(("since", Some("2018-08-20..2018-08-28"))))
            request = Request[IO](Method.GET, uri)
            response <- vacEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """[
              |  {
              |    "vacationId" : 3,
              |    "employeeId" : 1,
              |    "since" : "2018-08-21",
              |    "until" : "2018-08-27",
              |    "created" : "2018-05-01T00:00:00Z",
              |    "updated" : null
              |  }
              |]""".stripMargin

          } yield {
            response.status shouldEqual Ok
            responseBody shouldEqual expectedBody
          }
          
        }
      }
    }
  }

}

