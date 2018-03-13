package com.test.vacalendar.service

import cats.effect._
import org.scalatest._
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}

// import doobie.util.transactor.Transactor
// import doobie._
import doobie.hikari.implicits._

import com.vacalendar.conf.DatabaseConfig
import com.vacalendar.Module
import com.vacalendar.domain._
// import com.vacalendar.errors._
import com.vacalendar.endpoints.QryParams._
import com.vacalendar.validation.ServiceValidationInterpreter
import com.vacalendar.repository._
import com.vacalendar.service.EmployeeService


class EmployeeServiceSpec extends WordSpec
                         with Matchers
                         with ForAllTestContainer {

  override val container = PostgreSQLContainer()

  def withCtx(testThunk: (EmployeeRepoInterpreter[IO], EmployeeService[IO]) => IO[Unit]): Unit = {

    val dbConf = DatabaseConfig(driver = "org.postgresql.Driver",
                                url = container.jdbcUrl,
                                user = container.username,
                                password = container.password)

    val m = new Module[IO]()
    val xa = m.dbTransactor(dbConf).unsafeRunSync()

    val emplRepo = new EmployeeRepoInterpreter[IO](xa)
    val posRepo = new PositionRepoInterpreter[IO](xa)
    val emplService = new EmployeeService[IO](emplRepo, posRepo, ServiceValidationInterpreter)
    
    m.migrateDb(xa).unsafeRunSync()

    try {
      testThunk(emplRepo, emplService).unsafeRunSync()
    } finally {
      m.dropDb(xa).unsafeRunSync()
      xa.shutdown.unsafeRunSync()
    }
  }

  "Employee Service" when {

    "getEmpls with empty query params and empty db" should {
      
      "return empty list of employees" in withCtx { 
        (emplRepo, emplService) => {

          for {
            result <- emplService.getEmpls(EmplsQryParams()).value
          } yield {
            result shouldEqual Right(List())
          }
          
        }
      }
    }

    "getEmpls with empty query params and prepared db" should {
      
      "return list of employees in asc order by employee_id" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          val empl1 = emplRepo.createEmpl(emplIn1).value.flatMap {
            case Right(empl) => IO(empl)
            case Left(e) => IO(e)
          }
          .unsafeRunSync()
          
          val empl2 = emplRepo.createEmpl(emplIn2).value.flatMap {
            case Right(empl) => IO(empl)
            case Left(e) => IO(e)
          }
          .unsafeRunSync()

          for {
            result <- emplService.getEmpls(EmplsQryParams()).value
          } yield {
            result shouldEqual Right(List(empl1, empl2))
          }
        }
      }
    }

    "getEmpls with query params and prepared db" should {
      
      "return list of employees in asc order by position_id" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          val empl1 = emplRepo.createEmpl(emplIn1).value.flatMap {
            case Right(empl) => IO(empl)
            case Left(e) => IO(e)
          }
          .unsafeRunSync()
          
          val empl2 = emplRepo.createEmpl(emplIn2).value.flatMap {
            case Right(empl) => IO(empl)
            case Left(e) => IO(e)
          }
          .unsafeRunSync()

          val qryParams = EmplsQryParams(orderByParams = Some(OrderByParams(field = "position_id", asc = true)))

          for {
            result <- emplService.getEmpls(qryParams).value
          } yield {
            result shouldEqual Right(List(empl1, empl2))
          }
        }
      }
    }


  }
}