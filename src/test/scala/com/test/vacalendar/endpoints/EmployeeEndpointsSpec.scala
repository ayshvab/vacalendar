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
import org.http4s.dsl._

import com.vacalendar.conf.DatabaseConfig
import com.vacalendar.Module
import com.vacalendar.validation.{ QryParamsValidationInterpreter => QV, ServiceValidationInterpreter => SV }
import com.vacalendar.repository.{ EmployeeRepoInterpreter, PositionRepoInterpreter }
import com.vacalendar.service.EmployeeService
import com.vacalendar.endpoints._
import com.vacalendar.domain._

class EmployeeEndpointsSpec
  extends WordSpec
    with Matchers
    with ForAllTestContainer
    with Http4sDsl[IO] {

  import Http4sTestUtils._

  implicit val emplInDecoder = jsonOf[IO, EmployeeIn]

  override val container = PostgreSQLContainer()

  def withCtx(testThunk: (EmployeeRepoInterpreter[IO], PositionRepoInterpreter[IO]) => IO[Assertion]): Unit = {

    val dbConf = DatabaseConfig(driver = "org.postgresql.Driver",
                                url = container.jdbcUrl,
                                user = container.username,
                                password = container.password)

    val m = new Module[IO]()
    val xa = m.dbTransactor(dbConf).unsafeRunSync()

    val emplRepo = new EmployeeRepoInterpreter[IO](xa)
    val posRepo = new PositionRepoInterpreter[IO](xa)

    m.migrateDb(xa).unsafeRunSync()

    try {
      testThunk(emplRepo, posRepo).unsafeRunSync()
    } finally {
      m.dropDb(xa).unsafeRunSync()
      xa.shutdown.unsafeRunSync()
    }
  }

  "Get employees by query params" when {

    "employees not exist" should {
      "return empty list of employees" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val uri = Uri(path = s"/employees")
          val request = Request[IO](Method.GET, uri)
          for {
            response <- emplEndpoints.run(request)
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

    "employees exist" should {
      "return list of employees" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          val uri = Uri(path = s"/employees")
          val request = Request[IO](Method.GET, uri)
          for {
            _ <- emplRepo.createEmpl(emplIn1, clockCreate).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2, clockCreate).value.map(_.right.get)

            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """[
              |  {
              |    "employeeId" : 1,
              |    "firstName" : "John",
              |    "lastName" : "Doe",
              |    "positionId" : 1,
              |    "created" : "2018-05-01T00:00:00Z",
              |    "updated" : null
              |  },
              |  {
              |    "employeeId" : 2,
              |    "firstName" : "Jack",
              |    "lastName" : "Holmes",
              |    "positionId" : 2,
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

    "with query params" should {

      "return list of employees in asc order by position_id" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          for {
            _ <- emplRepo.createEmpl(emplIn1, clockCreate).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees", 
                      query = Query(("orderBy", Some("positionId"))))
            request = Request[IO](Method.GET, uri)
            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """[
              |  {
              |    "employeeId" : 1,
              |    "firstName" : "John",
              |    "lastName" : "Doe",
              |    "positionId" : 1,
              |    "created" : "2018-05-01T00:00:00Z",
              |    "updated" : null
              |  },
              |  {
              |    "employeeId" : 2,
              |    "firstName" : "Jack",
              |    "lastName" : "Holmes",
              |    "positionId" : 2,
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

      "return list of employees in desc order by positionId" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          for {
            _ <- emplRepo.createEmpl(emplIn1, clockCreate).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees", 
                      query = Query(("orderBy", Some("-positionId"))))
            request = Request[IO](Method.GET, uri)
            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """[
              |  {
              |    "employeeId" : 2,
              |    "firstName" : "Jack",
              |    "lastName" : "Holmes",
              |    "positionId" : 2,
              |    "created" : "2018-05-01T00:00:00Z",
              |    "updated" : null
              |  },
              |  {
              |    "employeeId" : 1,
              |    "firstName" : "John",
              |    "lastName" : "Doe",
              |    "positionId" : 1,
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

      "return list of employees in desc order by positionId and filtered by positionId" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          for {
            _ <- emplRepo.createEmpl(emplIn1, clockCreate).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees", 
                      query = Query(("orderBy", Some("-positionId")), ("positionId", Some("1"))))
            request = Request[IO](Method.GET, uri)
            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """[
              |  {
              |    "employeeId" : 1,
              |    "firstName" : "John",
              |    "lastName" : "Doe",
              |    "positionId" : 1,
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

      "return empty list of employees if nothing was filtered" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          for {
            _ <- emplRepo.createEmpl(emplIn1, clockCreate).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees", 
                      query = Query(("orderBy", Some("-positionId")), ("positionId", Some("1")), ("firstname", Some("Helen"))))
            request = Request[IO](Method.GET, uri)
            response <- emplEndpoints.run(request)
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

  }

  "Get employee" when {
    "employee not exist" should { 
      "Not Found" in withCtx { 
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          for {
            _ <- emplRepo.createEmpl(emplIn1, clockCreate).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2, clockCreate).value.map(_.right.get)

            nonExistingEmplId = 10
            uri = Uri(path = s"/employees/$nonExistingEmplId")
            request = Request[IO](Method.GET, uri)
            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
          } yield {
            response.status shouldEqual NotFound
          }
        }
      }
    }

    "employee exist" should {
      "return employee" in withCtx { 
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          for {
            _ <- emplRepo.createEmpl(emplIn1, clockCreate).value.map(_.right.get)
            empl2 <- emplRepo.createEmpl(emplIn2, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl2.employeeId}")
            request = Request[IO](Method.GET, uri)
            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "employeeId" : 2,
              |  "firstName" : "Jack",
              |  "lastName" : "Holmes",
              |  "positionId" : 2,
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
  }

  "Delete employee" when {
    "employee not exist" should {
      "return error Employee Not Found" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)

          for {
            _ <- emplRepo.createEmpl(emplIn1, clockCreate).value.map(_.right.get)
           
            nonExistingEmplId = 10
            uri = Uri(path = s"/employees/$nonExistingEmplId")
            request = Request[IO](Method.DELETE, uri)
            response <- emplEndpoints.run(request)
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
      "delete" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          for {
            _ <- emplRepo.createEmpl(emplIn1, clockCreate).value.map(_.right.get)
            empl2 <- emplRepo.createEmpl(emplIn2, clockCreate).value.map(_.right.get)
           
            uri = Uri(path = s"/employees/${empl2.employeeId}")
            request = Request[IO](Method.DELETE, uri)
            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))

            optFound <- emplRepo.getEmpl(empl2.employeeId).value
          } yield {
            optFound shouldEqual Right(None)
            response.status shouldEqual NoContent
          }
        }
      }
    }

  }

  "Create employee" when {
    "employee input has valid data" should {
      "create employee and return this employee" in withCtx { 
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val uri = Uri(path = s"/employees")

          for {
            request <- Request[IO](Method.POST, uri).withBody(emplIn.asJson)
            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "employeeId" : 1,
              |  "firstName" : "John",
              |  "lastName" : "Doe",
              |  "positionId" : 1,
              |  "created" : "2018-05-01T00:00:00Z",
              |  "updated" : null
              |}""".stripMargin

            created <- emplRepo.getEmpl(1).value.map(_.right.get)
          } yield {
            response.status shouldEqual Created
            responseBody shouldEqual expectedBody
            created shouldEqual Some(Employee(1, "John", "Doe", 1, Instant.parse("2018-05-01T00:00:00Z")))
          }
        }
      }
    }

    "employee input has invalid data" should {
      "don't create and return errors" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockCreate).service)

          val emplIn = EmployeeIn(firstName = "John@#", lastName = "Doe999", positionId = 10)
          
          val uri = Uri(path = s"/employees")
          for {
            request <- Request[IO](Method.POST, uri).withBody(emplIn.asJson)
            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "FirstNameHasSpecialCharacters",
              |        "message" : "First name: John@# has special characters"
              |      },
              |      {
              |        "code" : "LastNameHasSpecialCharacters",
              |        "message" : "Last name: Doe999 has special characters"
              |      },
              |      {
              |        "code" : "PosNotFound",
              |        "message" : "Position not found"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            created <- emplRepo.getEmpl(1).value.map(_.right.get)
          } yield {
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
            created shouldEqual None
          }
        }
      }
    }
  }

  "Update employee" when {
    "employee input has valid data and old employee exist" should {
      "update old employee and return new one" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val clockUpdate = Clock.fixed(LocalDate.of(2018, 6, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockUpdate).service)

          val emplInForCreate = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplInUpdate = EmployeeIn(firstName = "John", lastName = "Wayne", positionId = 1)

          for {
            empl <- emplService.createEmpl(emplInForCreate, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl.employeeId}")
            request <- Request[IO](Method.PATCH, uri).withBody(emplInUpdate.asJson)
            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "employeeId" : 1,
              |  "firstName" : "John",
              |  "lastName" : "Wayne",
              |  "positionId" : 1,
              |  "created" : "2018-05-01T00:00:00Z",
              |  "updated" : "2018-06-01T00:00:00Z"
              |}""".stripMargin

            updated <- emplRepo.getEmpl(empl.employeeId).value.map(_.right.get)
          } yield {
            response.status shouldEqual Ok
            responseBody shouldEqual expectedBody
            updated shouldEqual Some(Employee(1, "John", "Wayne", 1, Instant.parse("2018-05-01T00:00:00Z"), Some(Instant.parse("2018-06-01T00:00:00Z"))))
          }
        } 
      }
    }

    "employee input has invalid data" should {
      "don't update old employee and return errors" in withCtx {
        (emplRepo, posRepo) => {
          val clockCreate = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val clockUpdate = Clock.fixed(LocalDate.of(2018, 6, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockUpdate).service)

          val emplInForCreate = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val invalidEmplInUpdate = EmployeeIn(firstName = "John", lastName = "Wayne1", positionId = 10)

          for {
            empl <- emplService.createEmpl(emplInForCreate, clockCreate).value.map(_.right.get)

            uri = Uri(path = s"/employees/${empl.employeeId}")
            request <- Request[IO](Method.PATCH, uri).withBody(invalidEmplInUpdate.asJson)
            response <- emplEndpoints.run(request)
              .getOrElse(fail(s"Request was not handled: $request"))
            responseBody <- response.as[String]
            expectedBody =
              """{
              |  "error" : {
              |    "code" : "ServiceValidationErr",
              |    "message" : "Service has some validation errors",
              |    "details" : [
              |      {
              |        "code" : "LastNameHasSpecialCharacters",
              |        "message" : "Last name: Wayne1 has special characters"
              |      },
              |      {
              |        "code" : "PosNotFound",
              |        "message" : "Position not found"
              |      }
              |    ]
              |  }
              |}""".stripMargin

            updated <- emplRepo.getEmpl(empl.employeeId).value.map(_.right.get)
          } yield {
            response.status shouldEqual BadRequest
            responseBody shouldEqual expectedBody
            updated shouldEqual Some(empl)
          }
        } 
      }
    }

    "employee input has valid data but old employee don't exist" should {
      "return error Employee not found" in withCtx {
        (emplRepo, posRepo) => {
          val clockUpdate = Clock.fixed(LocalDate.of(2018, 6, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
          val emplService = new EmployeeService[IO](emplRepo, posRepo, SV)
          val errHandler = new EndpointErrorHandler[IO]()
          val emplEndpoints = ioMiddleware(new EmployeeEndpoints[IO](emplService, QV, errHandler, clockUpdate).service)
          
          val validEmplInUpdate = EmployeeIn(firstName = "John", lastName = "Wayne", positionId = 1)
          val nonExistingEmplId = 10
          val uri = Uri(path = s"/employees/${nonExistingEmplId}")
          for {
            request <- Request[IO](Method.PATCH, uri).withBody(validEmplInUpdate.asJson)
            response <- emplEndpoints.run(request)
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
    
  }
}
