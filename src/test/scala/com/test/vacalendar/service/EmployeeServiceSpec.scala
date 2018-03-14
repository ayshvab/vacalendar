package com.test.vacalendar.service

import cats.effect._
import cats.data._
import org.scalatest._
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import doobie.hikari.implicits._

import com.vacalendar.conf.DatabaseConfig
import com.vacalendar.Module
import com.vacalendar.domain._
import com.vacalendar.errors._
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
    val serviceValidation = new ServiceValidationInterpreter()
    val emplService = new EmployeeService[IO](emplRepo, posRepo, serviceValidation)
    
    m.migrateDb(xa).unsafeRunSync()

    try {
      testThunk(emplRepo, emplService).unsafeRunSync()
    } finally {
      m.dropDb(xa).unsafeRunSync()
      xa.shutdown.unsafeRunSync()
    }
  }

  "Get employees by query params" when {

    "with empty query params and empty db" should {
      
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

    "with empty query params and prepared db" should {
      
      "return list of employees in asc order by employee_id" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          val empl1 = emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            .unsafeRunSync()          
          val empl2 = emplRepo.createEmpl(emplIn2).value.map(_.right.get)
            .unsafeRunSync()

          for {
            result <- emplService.getEmpls(EmplsQryParams()).value
          } yield {
            result shouldEqual Right(List(empl1, empl2))
          }
        }
      }
    }

    "with query params and prepared db" should {
      
      "return list of employees in asc order by position_id" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          val empl1 = emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            .unsafeRunSync()          
          val empl2 = emplRepo.createEmpl(emplIn2).value.map(_.right.get)
            .unsafeRunSync()

          val qryParams = EmplsQryParams(orderByParams = Some(OrderByParams(field = "position_id", asc = true)))

          for {
            result <- emplService.getEmpls(qryParams).value
          } yield {
            result shouldEqual Right(List(empl1, empl2))
          }
        }
      }

      "return list of employees in desc order by positionId" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          val empl1 = emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            .unsafeRunSync()
          val empl2 = emplRepo.createEmpl(emplIn2).value.map(_.right.get)
            .unsafeRunSync()

          val qryParams = EmplsQryParams(orderByParams = Some(OrderByParams(field = "position_id", asc = false)))

          for {
            result <- emplService.getEmpls(qryParams).value
          } yield {
            result shouldEqual Right(List(empl2, empl1))
          }
        }
      }

      "return list of employees in desc order by positionId and filtered by positionId" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          val empl1 = emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            .unsafeRunSync()
          
          emplRepo.createEmpl(emplIn2).value.map(_.right.get)
            .unsafeRunSync()

          val qryParams = EmplsQryParams(orderByParams = Some(OrderByParams(field = "position_id", asc = false)), positionId = Some(1))

          for {
            result <- emplService.getEmpls(qryParams).value
          } yield {
            result shouldEqual Right(List(empl1))
          }
        }
      }

      "return empty list of employees if nothing was filtered" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            .unsafeRunSync()
          
          emplRepo.createEmpl(emplIn2).value.map(_.right.get)
            .unsafeRunSync()

          val qryParams = EmplsQryParams(orderByParams = Some(OrderByParams(field = "position_id", asc = false)), firstName = Some("Helen"), positionId = Some(1))

          for {
            result <- emplService.getEmpls(qryParams).value
          } yield {
            result shouldEqual Right(List())
          }
        }
      }
    }

  }

  "Get employee" when {

    "employee with employeeId not exist" should {
      
      "return None" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          emplRepo.createEmpl(emplIn1).value.unsafeRunSync()
          emplRepo.createEmpl(emplIn2).value.unsafeRunSync()
          
          for {
            result <- emplService.getEmpl(10).value
          } yield {
            result shouldEqual Right(None)
          }
        }
      }
    }

    "employee with employeeId exist" should {
      
      "return Some employee" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          emplRepo.createEmpl(emplIn1).value.unsafeRunSync()

          val empl2 = emplRepo.createEmpl(emplIn2).value.map(_.right.get)
            .unsafeRunSync()
          
          for {
            result <- emplService.getEmpl(2).value
          } yield {
            result shouldEqual Right(Some(empl2))
          }
        }
      }
    }

  }

  "Delete employee" when {

    "employee with employeeId not exist" should {
      
      "return error Employee Not Found" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)

          emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            .unsafeRunSync()
          
          for {
            result <- emplService.deleteEmpl(10).value
          } yield {
            result shouldEqual Left(AppError.ServiceValidationErrWrapper(EmplNotFound))
          }
        }
      }
    }

    "employee with employeeId exist" should {
      
      "delete employee" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 2)

          emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            .unsafeRunSync()

          val empl2 = emplRepo.createEmpl(emplIn2).value.map(_.right.get)
            .unsafeRunSync()
          
          for {
            deleted <- emplService.deleteEmpl(2).value
            optFound <- emplRepo.getEmpl(2).value
          } yield {
            deleted shouldEqual Right(empl2)
            optFound shouldEqual Right(None)
          }
        }
      }
    }

  }

  "Create employee" when {

    "employee input has valid data" should {
      
      "create employee and return this employee" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)

          val empl = emplService.createEmpl(emplIn).value.map(_.right.get)
            .unsafeRunSync()
          
          for {
            result <- emplRepo.getEmpl(1).value
          } yield {
            result shouldEqual Right(Some(empl))
          }
        }
      }
    }

    "employee input has invalid data" should {
      
      "don't create employee and return validation errors" in withCtx { 
        (emplRepo, emplService) => {

          val emplIn = EmployeeIn(firstName = "John@#", lastName = "Doe999", positionId = 10)

          val errors = emplService.createEmpl(emplIn).value.map(_.left.get)
            .unsafeRunSync()
          
          for {
            result <- emplRepo.getEmpl(1).value
          } yield {
            result shouldEqual Right(None)
            errors shouldEqual AppError.ServiceValidationErrsWrapper(
              NonEmptyList(
                FirstNameHasSpecialCharacters("John@#"), 
                List(LastNameHasSpecialCharacters("Doe999"), PosNotFound)))
          }
        }
      }
    }

  }


  "Update employee" when {

    "employee input has valid data and old employee exist in db" should {

      "update old employee and return new one" in withCtx {
        (emplRepo, emplService) => {

          val emplInForCreate = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)

          val empl = emplService.createEmpl(emplInForCreate).value.map(_.right.get)
            .unsafeRunSync()

          val emplInUpdate = EmployeeIn(firstName = "John", lastName = "Wayne", positionId = 1)

          val updated = emplService.updateEmpl(empl.employeeId, emplInUpdate).value.map(_.right.get)
            .unsafeRunSync()

          for {
            result <- emplRepo.getEmpl(updated.employeeId).value
          } yield {
            result shouldEqual Right(Some(updated))
          }
        }
      }
    }

    "employee input has invalid data and old employee exist in db" should {

      "don't update old employee and return errors/error" in withCtx {
        (emplRepo, emplService) => {

          val emplInForCreate = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)

          val empl = emplService.createEmpl(emplInForCreate).value.map(_.right.get)
            .unsafeRunSync()

          val invalidEmplInUpdate = EmployeeIn(firstName = "John", lastName = "Wayne1", positionId = 10)

          val errors = emplService.updateEmpl(empl.employeeId, invalidEmplInUpdate).value.map(_.left.get)
            .unsafeRunSync()

          for {
            oldEmpl <- emplRepo.getEmpl(empl.employeeId).value
          } yield {
            Right(Some(empl)) shouldEqual oldEmpl
            errors shouldEqual AppError.ServiceValidationErrsWrapper(
              NonEmptyList(
                LastNameHasSpecialCharacters("Wayne1"), List(PosNotFound)))
          }
        }
      }
    }

    "employee input has valid data but old employee don't exist in db" should {

      "return error Employee not found" in withCtx {
        (emplRepo, emplService) => {

          val validEmplInUpdate = EmployeeIn(firstName = "John", lastName = "Wayne", positionId = 1)

          for {
            error <- emplService.updateEmpl(1, validEmplInUpdate).value
          } yield {
            error shouldEqual Left(AppError.ServiceValidationErrWrapper(EmplNotFound))
          }          
        }
      }
    }
  }

}