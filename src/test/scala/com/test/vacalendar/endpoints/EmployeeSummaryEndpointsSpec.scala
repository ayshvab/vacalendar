package com.test.vacalendar.endpoints

import java.time._
import cats.effect._
import org.scalatest._
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}

import doobie.hikari.implicits._

import io.circe.generic.auto._
import org.http4s.circe._
import org.http4s._
import org.http4s.dsl._

import com.vacalendar.conf.DatabaseConfig
import com.vacalendar.Module
import com.vacalendar.validation.{ QryParamsValidationInterpreter => QV , ServiceValidationInterpreter => SV, ValidationRules}
import com.vacalendar.repository._
import com.vacalendar.service._
import com.vacalendar.endpoints._
import com.vacalendar.domain._

class EmployeeSummaryEndpointsSpec
  extends WordSpec
    with Matchers
    with ForAllTestContainer
    with Http4sDsl[IO] {

  import Http4sTestUtils._

  implicit val emplInDecoder = jsonOf[IO, EmployeeIn]

  override val container = PostgreSQLContainer()

  def withCtx(testThunk: (EmployeeService[IO],
                          VacationService[IO],
                          EmployeeSummaryService[IO]) => IO[Assertion]): Unit = {

    val dbConf = DatabaseConfig(driver = "org.postgresql.Driver",
                                url = container.jdbcUrl,
                                user = container.username,
                                password = container.password)

    val m = new Module[IO]()
    val xa = m.dbTransactor(dbConf).unsafeRunSync()

    val emplRepo = new EmployeeRepoInterpreter[IO](xa)
    val posRepo = new PositionRepoInterpreter[IO](xa)
    val vacRepo = new VacationRepoInterpreter[IO](xa)
    val emplSummaryRepo = new EmployeeSummaryRepoInterpreter[IO](xa)

    val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
    val vacService = new VacationService[IO](vacRepo, emplRepo, SV)
    val emplSummaryService = new EmployeeSummaryService[IO](emplSummaryRepo)

    m.migrateDb(xa).unsafeRunSync()

    try {
      testThunk(emplService, vacService, emplSummaryService).unsafeRunSync()
    } finally {
      m.dropDb(xa).unsafeRunSync()
      xa.shutdown.unsafeRunSync()
    }
  }

    "Get employee summary" when {
      "employee not exist" should {
        "return Employee Not Found" in withCtx {
          (emplService, vacService, emplSummaryService) => {
            val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
            val errHandler = new EndpointErrorHandler[IO]()
            val emplSummaryEndpoints = ioMiddleware(new EmployeeSummaryEndpoints[IO](emplSummaryService, QV, errHandler, clock).service)

            val nonExistingEmplId = 1
            val uri = Uri(path = s"/employees/view=summary/$nonExistingEmplId")
            val request = Request[IO](Method.GET, uri)

            for {
              response <- emplSummaryEndpoints.run(request)
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

      "employee exist" should {
        "return employee summary" in withCtx {
          (emplService, vacService, emplSummaryService) => {
            val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
            val clockOfEmplCreate = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
            val clockOfVacsCreate = Clock.fixed(LocalDate.of(2018, 2, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
            val errHandler = new EndpointErrorHandler[IO]()
            val emplSummaryEndpoints = ioMiddleware(new EmployeeSummaryEndpoints[IO](emplSummaryService, QV, errHandler, clock).service)

            val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
            val emplIn2 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)

            val currVacIn = VacationIn(since = LocalDate.now(clock), until = LocalDate.now(clock).plusDays(5))
            val currVacDays = currVacIn.until.toEpochDay - currVacIn.since.toEpochDay
            val pastVacIn = VacationIn(since = LocalDate.of(2018, 3, 1), until = LocalDate.of(2018, 3, 8))
            val pastVacDays = pastVacIn.until.toEpochDay - pastVacIn.since.toEpochDay
            val futureVacIn = VacationIn(since = LocalDate.of(2018, 8, 1), until = LocalDate.of(2018, 8, 10))
            val futureVacDays = futureVacIn.until.toEpochDay - futureVacIn.since.toEpochDay
            val remainedVacationDaysCount = (ValidationRules.vacDaysMaxCountPerYear - (currVacDays + pastVacDays + futureVacDays))
            for {
              empl1 <- emplService.createEmpl(emplIn1, clockOfEmplCreate).value.map(_.right.get)
              empl2 <- emplService.createEmpl(emplIn2, clockOfEmplCreate).value.map(_.right.get)

              currVac <- vacService.createVac(empl1.employeeId, currVacIn, clockOfVacsCreate)
                .value.map(_.right.get)
              pastVac <- vacService.createVac(empl1.employeeId, pastVacIn, clockOfVacsCreate)
                .value.map(_.right.get)
              futureVac <- vacService.createVac(empl1.employeeId, futureVacIn, clockOfVacsCreate)
                .value.map(_.right.get)

              uri = Uri(path = s"/employees/view=summary/${empl1.employeeId}")
              request = Request[IO](Method.GET, uri)
              response <- emplSummaryEndpoints.run(request)
                .getOrElse(fail(s"Request was not handled: $request"))
              responseBody <- response.as[String]
              expectedBody =
                s"""{
                |  "employeeId" : 1,
                |  "firstName" : "John",
                |  "lastName" : "Doe",
                |  "positionId" : 1,
                |  "positionTitle" : "developer",
                |  "remainedVacationDaysCount" : $remainedVacationDaysCount,
                |  "isOnVacation" : true,
                |  "pastVacations" : [
                |    {
                |      "vacationId" : 2,
                |      "employeeId" : 1,
                |      "since" : "2018-03-01",
                |      "until" : "2018-03-08",
                |      "created" : "2018-02-01T00:00:00Z",
                |      "updated" : null
                |    }
                |  ],
                |  "currentVacation" : {
                |    "vacationId" : 1,
                |    "employeeId" : 1,
                |    "since" : "2018-05-01",
                |    "until" : "2018-05-06",
                |    "created" : "2018-02-01T00:00:00Z",
                |    "updated" : null
                |  },
                |  "futureVacations" : [
                |    {
                |      "vacationId" : 3,
                |      "employeeId" : 1,
                |      "since" : "2018-08-01",
                |      "until" : "2018-08-10",
                |      "created" : "2018-02-01T00:00:00Z",
                |      "updated" : null
                |    }
                |  ]
                |}""".stripMargin
            } yield {
              response.status shouldEqual Ok
              responseBody shouldEqual expectedBody
            }
          }
        }
      }
    }

    "Get employee summaries by query params" when {
      "with empty qryParams" should {
        "return list of employee summaries in default order" in withCtx {
         (emplService, vacService, emplSummaryService) => {
            val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
            val clockOfEmplCreate = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
            val clockOfVacsCreate = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
            val errHandler = new EndpointErrorHandler[IO]()
            val emplSummaryEndpoints = ioMiddleware(new EmployeeSummaryEndpoints[IO](emplSummaryService, QV, errHandler, clock).service)

            val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
            val emplIn2 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)

            val empl1CurrVacIn = VacationIn(since = LocalDate.now(clock), until = LocalDate.now(clock).plusDays(5))
            val empl1CurrVacDays = empl1CurrVacIn.until.toEpochDay - empl1CurrVacIn.since.toEpochDay
            val empl1PastVacIn = VacationIn(since = LocalDate.of(2018, 2, 1), until = LocalDate.of(2018, 2, 8))
            val empl1PastVacDays = empl1PastVacIn.until.toEpochDay - empl1PastVacIn.since.toEpochDay
            val empl1FutureVacIn = VacationIn(since = LocalDate.of(2018, 8, 1), until = LocalDate.of(2018, 8, 10))
            val empl1FutureVacDays = empl1FutureVacIn.until.toEpochDay - empl1FutureVacIn.since.toEpochDay

            val empl1RemainedVacationDaysCount = ValidationRules
              .vacDaysMaxCountPerYear - (empl1CurrVacDays + empl1PastVacDays + empl1FutureVacDays)

            val empl2PastVacIn1 = VacationIn(since = LocalDate.of(2018, 4, 1), until = LocalDate.of(2018, 4, 6))
            val empl2PastVacIn1Days = empl2PastVacIn1.until.toEpochDay - empl2PastVacIn1.since.toEpochDay
            val empl2PastVacIn2 = VacationIn(since = LocalDate.of(2018, 3, 1), until = LocalDate.of(2018, 3, 10))
            val empl2PastVacIn2Days = empl2PastVacIn2.until.toEpochDay - empl2PastVacIn2.since.toEpochDay
            val empl2FutureVacIn = VacationIn(since = LocalDate.of(2018, 9, 1), until = LocalDate.of(2018, 9, 10))
            val empl2FutureVacDays = empl2FutureVacIn.until.toEpochDay - empl2FutureVacIn.since.toEpochDay

            val empl2RemainedVacationDaysCount = ValidationRules
              .vacDaysMaxCountPerYear - (empl2PastVacIn1Days + empl2PastVacIn2Days + empl2FutureVacDays)

            for {
              empl1 <- emplService.createEmpl(emplIn1, clockOfEmplCreate).value.map(_.right.get)
              empl2 <- emplService.createEmpl(emplIn2, clockOfEmplCreate).value.map(_.right.get)

              empl1CurrVac <- vacService.createVac(empl1.employeeId, empl1CurrVacIn, clockOfVacsCreate)
                .value.map(_.right.get)
              empl1PastVac <- vacService.createVac(empl1.employeeId, empl1PastVacIn, clockOfVacsCreate)
                .value.map(_.right.get)
              empl1FutureVac <- vacService.createVac(empl1.employeeId, empl1FutureVacIn, clockOfVacsCreate)
                .value.map(_.right.get)

              empl2PastVac1 <- vacService.createVac(empl2.employeeId, empl2PastVacIn1, clockOfVacsCreate)
                .value.map(_.right.get)
              empl2PastVac2 <- vacService.createVac(empl2.employeeId, empl2PastVacIn2, clockOfVacsCreate)
                .value.map(_.right.get)
              empl2FutureVac <- vacService.createVac(empl2.employeeId, empl2FutureVacIn, clockOfVacsCreate)
                .value.map(_.right.get)

              uri = Uri(path = s"/employees/view=summary")
              request = Request[IO](Method.GET, uri)
              response <- emplSummaryEndpoints.run(request)
                .getOrElse(fail(s"Request was not handled: $request"))
              responseBody <- response.as[String]
              expectedBody =
                s"""[
                |  {
                |    "employeeId" : 1,
                |    "firstName" : "John",
                |    "lastName" : "Doe",
                |    "positionId" : 1,
                |    "positionTitle" : "developer",
                |    "remainedVacationDaysCount" : $empl1RemainedVacationDaysCount,
                |    "isOnVacation" : true,
                |    "pastVacations" : [
                |      {
                |        "vacationId" : 2,
                |        "employeeId" : 1,
                |        "since" : "2018-02-01",
                |        "until" : "2018-02-08",
                |        "created" : "2018-01-01T00:00:00Z",
                |        "updated" : null
                |      }
                |    ],
                |    "currentVacation" : {
                |      "vacationId" : 1,
                |      "employeeId" : 1,
                |      "since" : "2018-05-01",
                |      "until" : "2018-05-06",
                |      "created" : "2018-01-01T00:00:00Z",
                |      "updated" : null
                |    },
                |    "futureVacations" : [
                |      {
                |        "vacationId" : 3,
                |        "employeeId" : 1,
                |        "since" : "2018-08-01",
                |        "until" : "2018-08-10",
                |        "created" : "2018-01-01T00:00:00Z",
                |        "updated" : null
                |      }
                |    ]
                |  },
                |  {
                |    "employeeId" : 2,
                |    "firstName" : "Peter",
                |    "lastName" : "Parker",
                |    "positionId" : 1,
                |    "positionTitle" : "developer",
                |    "remainedVacationDaysCount" : $empl2RemainedVacationDaysCount,
                |    "isOnVacation" : false,
                |    "pastVacations" : [
                |      {
                |        "vacationId" : 5,
                |        "employeeId" : 2,
                |        "since" : "2018-03-01",
                |        "until" : "2018-03-10",
                |        "created" : "2018-01-01T00:00:00Z",
                |        "updated" : null
                |      },
                |      {
                |        "vacationId" : 4,
                |        "employeeId" : 2,
                |        "since" : "2018-04-01",
                |        "until" : "2018-04-06",
                |        "created" : "2018-01-01T00:00:00Z",
                |        "updated" : null
                |      }
                |    ],
                |    "currentVacation" : null,
                |    "futureVacations" : [
                |      {
                |        "vacationId" : 6,
                |        "employeeId" : 2,
                |        "since" : "2018-09-01",
                |        "until" : "2018-09-10",
                |        "created" : "2018-01-01T00:00:00Z",
                |        "updated" : null
                |      }
                |    ]
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

    "with qryParams" should {
      "return ordered and filterd list of employee summaries 1" in withCtx {
        (emplService, vacService, emplSummaryService) => {
            val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
            val clockOfEmplCreate = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
            val clockOfVacsCreate = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
            val errHandler = new EndpointErrorHandler[IO]()
            val emplSummaryEndpoints = ioMiddleware(new EmployeeSummaryEndpoints[IO](emplSummaryService, QV, errHandler, clock).service)

            val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
            val emplIn2 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
            val emplIn3 = EmployeeIn(firstName = "Montgomery", lastName = "Montgomery", positionId = 2)

            val empl1CurrVacIn = VacationIn(since = LocalDate.now(clock), until = LocalDate.now(clock).plusDays(5))
            val empl1PastVacIn = VacationIn(since = LocalDate.of(2018, 2, 1), until = LocalDate.of(2018, 2, 8))
            val empl1FutureVacIn = VacationIn(since = LocalDate.of(2018, 8, 1), until = LocalDate.of(2018, 8, 10))

            val empl2PastVacIn1 = VacationIn(since = LocalDate.of(2018, 4, 1), until = LocalDate.of(2018, 4, 6))
            val empl2PastVacIn1Days = empl2PastVacIn1.until.toEpochDay - empl2PastVacIn1.since.toEpochDay
            val empl2PastVacIn2 = VacationIn(since = LocalDate.of(2018, 3, 1), until = LocalDate.of(2018, 3, 10))
            val empl2PastVacIn2Days = empl2PastVacIn2.until.toEpochDay - empl2PastVacIn2.since.toEpochDay
            val empl2FutureVacIn = VacationIn(since = LocalDate.of(2018, 9, 1), until = LocalDate.of(2018, 9, 10))
            val empl2FutureVacDays = empl2FutureVacIn.until.toEpochDay - empl2FutureVacIn.since.toEpochDay

            val empl2RemainedVacationDaysCount = ValidationRules
              .vacDaysMaxCountPerYear - (empl2PastVacIn1Days + empl2PastVacIn2Days + empl2FutureVacDays)

            for {
              empl1 <- emplService.createEmpl(emplIn1, clockOfEmplCreate).value.map(_.right.get)
              empl2 <- emplService.createEmpl(emplIn2, clockOfEmplCreate).value.map(_.right.get)
              empl3 <- emplService.createEmpl(emplIn3, clockOfEmplCreate).value.map(_.right.get)
              emplId1 = empl1.employeeId
              emplId2 = empl2.employeeId

              _ <- vacService.createVac(emplId1, empl1CurrVacIn, clockOfVacsCreate).value
              _ <- vacService.createVac(emplId1, empl1PastVacIn, clockOfVacsCreate).value
              _ <- vacService.createVac(emplId1, empl1FutureVacIn, clockOfVacsCreate).value

              empl2PastVac1 <- vacService.createVac(emplId2, empl2PastVacIn1, clockOfVacsCreate).value.map(_.right.get)
              empl2PastVac2 <- vacService.createVac(emplId2, empl2PastVacIn2, clockOfVacsCreate).value.map(_.right.get)
              empl2FutureVac <- vacService.createVac(emplId2, empl2FutureVacIn, clockOfVacsCreate).value.map(_.right.get)

              uri = Uri(path = s"/employees/view=summary",
                        query = Query(("orderBy", Some("remainedVacationDays")),
                                      ("isOnVacation", Some("f"))))
              request = Request[IO](Method.GET, uri)
              response <- emplSummaryEndpoints.run(request)
                .getOrElse(fail(s"Request was not handled: $request"))
              responseBody <- response.as[String]
              expectedBody =
                s"""[
                |  {
                |    "employeeId" : 2,
                |    "firstName" : "Peter",
                |    "lastName" : "Parker",
                |    "positionId" : 1,
                |    "positionTitle" : "developer",
                |    "remainedVacationDaysCount" : $empl2RemainedVacationDaysCount,
                |    "isOnVacation" : false,
                |    "pastVacations" : [
                |      {
                |        "vacationId" : 5,
                |        "employeeId" : 2,
                |        "since" : "2018-03-01",
                |        "until" : "2018-03-10",
                |        "created" : "2018-01-01T00:00:00Z",
                |        "updated" : null
                |      },
                |      {
                |        "vacationId" : 4,
                |        "employeeId" : 2,
                |        "since" : "2018-04-01",
                |        "until" : "2018-04-06",
                |        "created" : "2018-01-01T00:00:00Z",
                |        "updated" : null
                |      }
                |    ],
                |    "currentVacation" : null,
                |    "futureVacations" : [
                |      {
                |        "vacationId" : 6,
                |        "employeeId" : 2,
                |        "since" : "2018-09-01",
                |        "until" : "2018-09-10",
                |        "created" : "2018-01-01T00:00:00Z",
                |        "updated" : null
                |      }
                |    ]
                |  },
                |  {
                |    "employeeId" : 3,
                |    "firstName" : "Montgomery",
                |    "lastName" : "Montgomery",
                |    "positionId" : 2,
                |    "positionTitle" : "designer",
                |    "remainedVacationDaysCount" : ${ValidationRules.vacDaysMaxCountPerYear},
                |    "isOnVacation" : false,
                |    "pastVacations" : [
                |    ],
                |    "currentVacation" : null,
                |    "futureVacations" : [
                |    ]
                |  }
                |]""".stripMargin
                
            } yield {
              response.status shouldEqual Ok
              responseBody shouldEqual expectedBody
            }
          } 
      }

      "return ordered and filterd list of employee summaries 2" in withCtx {
        (emplService, vacService, emplSummaryService) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val clockOfEmplCreate = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val clockOfVacsCreate = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplSummaryEndpoints = ioMiddleware(new EmployeeSummaryEndpoints[IO](emplSummaryService, QV, errHandler, clock).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn3 = EmployeeIn(firstName = "Montgomery", lastName = "Montgomery", positionId = 2)

          val empl1CurrVacIn = VacationIn(since = LocalDate.now(clock), until = LocalDate.now(clock).plusDays(5))
          val empl1CurrVacInDays = empl1CurrVacIn.until.toEpochDay - empl1CurrVacIn.since.toEpochDay
          val empl1PastVacIn = VacationIn(since = LocalDate.of(2018, 2, 1), until = LocalDate.of(2018, 2, 8))
          val empl1PastVacInDays = empl1PastVacIn.until.toEpochDay - empl1PastVacIn.since.toEpochDay
          val empl1FutureVacIn = VacationIn(since = LocalDate.of(2018, 8, 1), until = LocalDate.of(2018, 8, 10))
          val empl1FutureVacInDays = empl1FutureVacIn.until.toEpochDay - empl1FutureVacIn.since.toEpochDay

          val empl1RemainedVacationDaysCount = ValidationRules
            .vacDaysMaxCountPerYear - (empl1PastVacInDays + empl1CurrVacInDays + empl1FutureVacInDays)

          val empl2PastVacIn1 = VacationIn(since = LocalDate.of(2018, 4, 1), until = LocalDate.of(2018, 4, 6))
          val empl2PastVacIn2 = VacationIn(since = LocalDate.of(2018, 3, 1), until = LocalDate.of(2018, 3, 10))
          val empl2FutureVacIn = VacationIn(since = LocalDate.of(2018, 9, 1), until = LocalDate.of(2018, 9, 10))

          for {
            empl1 <- emplService.createEmpl(emplIn1, clockOfEmplCreate).value.map(_.right.get)
            empl2 <- emplService.createEmpl(emplIn2, clockOfEmplCreate).value.map(_.right.get)
            empl3 <- emplService.createEmpl(emplIn3, clockOfEmplCreate).value.map(_.right.get)
            emplId1 = empl1.employeeId
            emplId2 = empl2.employeeId

            _ <- vacService.createVac(emplId1, empl1CurrVacIn, clockOfVacsCreate).value
            _ <- vacService.createVac(emplId1, empl1PastVacIn, clockOfVacsCreate).value
            _ <- vacService.createVac(emplId1, empl1FutureVacIn, clockOfVacsCreate).value

            empl2PastVac1 <- vacService.createVac(emplId2, empl2PastVacIn1, clockOfVacsCreate).value.map(_.right.get)
            empl2PastVac2 <- vacService.createVac(emplId2, empl2PastVacIn2, clockOfVacsCreate).value.map(_.right.get)
            empl2FutureVac <- vacService.createVac(emplId2, empl2FutureVacIn, clockOfVacsCreate).value.map(_.right.get)
            
            uri = Uri(path = s"/employees/view=summary",
                      query = Query(("orderBy", Some("remainedVacationDays")),
                                    ("pastVacs.since", Some("2018-01-31..2018-06-01")),
                                    ("futureVacs.until", Some("2018-08-10"))))
            request = Request[IO](Method.GET, uri)
            response <- emplSummaryEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              s"""[
              |  {
                |    "employeeId" : 1,
                |    "firstName" : "John",
                |    "lastName" : "Doe",
                |    "positionId" : 1,
                |    "positionTitle" : "developer",
                |    "remainedVacationDaysCount" : $empl1RemainedVacationDaysCount,
                |    "isOnVacation" : true,
                |    "pastVacations" : [
                |      {
                |        "vacationId" : 2,
                |        "employeeId" : 1,
                |        "since" : "2018-02-01",
                |        "until" : "2018-02-08",
                |        "created" : "2018-01-01T00:00:00Z",
                |        "updated" : null
                |      }
                |    ],
                |    "currentVacation" : {
                |      "vacationId" : 1,
                |      "employeeId" : 1,
                |      "since" : "2018-05-01",
                |      "until" : "2018-05-06",
                |      "created" : "2018-01-01T00:00:00Z",
                |      "updated" : null
                |    },
                |    "futureVacations" : [
                |      {
                |        "vacationId" : 3,
                |        "employeeId" : 1,
                |        "since" : "2018-08-01",
                |        "until" : "2018-08-10",
                |        "created" : "2018-01-01T00:00:00Z",
                |        "updated" : null
                |      }
                |    ]
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