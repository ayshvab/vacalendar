package com.test.vacalendar.service

import java.time._
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
import com.vacalendar.service.VacationService

class VacationServiceSpec extends WordSpec
                            with Matchers
                            with ForAllTestContainer {

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

    "employee with employeeId not exist" should {

      "return error Employee Not Found" in withCtx {
        (emplRepo, vacRepo) => {
          val serviceValidation = new ServiceValidationInterpreter()
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val nonExistingEmplId: Long = 1
          val vacIn = VacationIn(since = LocalDate.of(2018, 3, 1), until = LocalDate.of(2018, 3, 10))

          for {
            result <- vacService.createVac(nonExistingEmplId, vacIn).value
          } yield {
            result shouldEqual Left(AppError.ServiceValidationErrWrapper(EmplNotFound))
          }
        }
      }
    }

    "vacation input has invalid data" should {

      "fail on basic validation" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val vacIn1 = VacationIn(since = LocalDate.of(2018, 1, 20), until = LocalDate.of(2017, 12, 25))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 20), until = LocalDate.of(2018, 7, 21))

          for {
            empl <- emplRepo.createEmpl(emplIn).value.map(_.right.get)

            result1 <- vacService.createVac(empl.employeeId, vacIn1).value
            result2 <- vacService.createVac(empl.employeeId, vacIn2).value
            createdVacs <- vacRepo.getVacs(empl.employeeId, VacsQryParams()).value
          } yield {
            createdVacs shouldEqual Right(List())

            result1 shouldEqual Left(
              AppError.ServiceValidationErrsWrapper(
                NonEmptyList(VacSinceDateMustBeBeforeUntilDate,
                           List(VacOnlyInFuture, VacMustStartAndEndWithin1Year, VacPeriodIsMoreMax))
              )
            )

            result2 shouldEqual Left(
              AppError.ServiceValidationErrsWrapper(
                NonEmptyList.one(VacPeriodIsLessMin)
              )
            )
          }
        }
      }

      "fail on validation creation: too many employees of one position will on vac in the same time" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

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

            _ <- vacService.createVac(empl1.employeeId, vacIn1).value
            _ <- vacService.createVac(empl2.employeeId, vacIn2).value
            vac3 <- vacService.createVac(empl3.employeeId, vacIn3).value.map(_.right.get)
            _ <- vacService.createVac(empl6.employeeId, vacIn3).value

            result <- vacService.createVac(empl3.employeeId, vacIn1).value
            created <- vacRepo.getVacs(empl3.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac3))
            result shouldEqual Left(AppError.ServiceValidationErrsWrapper(NonEmptyList.one(TooManyEmplsOfOnePosOnVac)))
          }

        }
      }

      "fail on validation creation: max count vacation days per year exceeded" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 6, 15), until = LocalDate.of(2018, 6, 30))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 8, 15), until = LocalDate.of(2018, 8, 30))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1).value.map(_.right.get)

            result <- vacService.createVac(empl1.employeeId, vacIn2).value
            created <- vacRepo.getVacs(empl1.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac1))
            result shouldEqual Left(AppError.ServiceValidationErrsWrapper(NonEmptyList.one(MaxCountVacDaysPerYearExceeded)))
          }
        }
      }

      "fail on validation creation: not enough days from new vacation to next vacation" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 6, 20), until = LocalDate.of(2018, 6, 30))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1).value.map(_.right.get)

            result <- vacService.createVac(empl1.employeeId, vacIn2).value
            created <- vacRepo.getVacs(empl1.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac1))
            result shouldEqual Left(AppError.ServiceValidationErrsWrapper(NonEmptyList.one(NotEnoughDaysToNextVac)))
          }

        }
      }

      "fail on validation creation: not enough days pass from prev vacation to new vacation" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 11))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 15), until = LocalDate.of(2018, 7, 26))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1).value.map(_.right.get)

            result <- vacService.createVac(empl1.employeeId, vacIn2).value
            created <- vacRepo.getVacs(empl1.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac1))
            result shouldEqual Left(AppError.ServiceValidationErrsWrapper(NonEmptyList.one(NotEnoughDaysPassFromLastVac)))
          }

        }
      }

      "fail on validation creation: not enough days between prev vacation/new vacation and between new vacation/next vacation" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 20), until = LocalDate.of(2018, 7, 25))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 7, 9), until = LocalDate.of(2018, 7, 16))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1).value.map(_.right.get)
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2).value.map(_.right.get)

            result <- vacService.createVac(empl1.employeeId, vacIn3).value
            created <- vacRepo.getVacs(empl1.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac1, vac2))
            result shouldEqual Left(
              AppError.ServiceValidationErrsWrapper(NonEmptyList(NotEnoughDaysPassFromLastVac, List(NotEnoughDaysToNextVac)))
            )
          }

        }
      }

      "fail on validation creation: vacations should not overlapped" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 10))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 8), until = LocalDate.of(2018, 7, 15))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1).value.map(_.right.get)

            result <- vacService.createVac(empl1.employeeId, vacIn2).value
            created <- vacRepo.getVacs(empl1.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac1))
            result shouldEqual Left(
              AppError.ServiceValidationErrsWrapper(NonEmptyList.one(VacsMustNotOverlap))
            )
          }

        }
      }
    }

    "valid vacation input" should {

      "create" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          
          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            created <- vacService.createVac(empl1.employeeId, vacIn).value.map(_.right.get)
            result <- vacRepo.getVac(empl1.employeeId, created.vacationId).value
          } yield {
            Right(Some(created)) shouldEqual result
          }
        }
      }
    }
  }

  "Update vacation" when {

    "vacation or employee not found" should {

      "fail if employee not found" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val vacIn = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 10))

          for {
            result <- vacService.updateVac(1, 1, vacIn).value
          } yield {
            result shouldEqual Left(AppError.ServiceValidationErrWrapper(EmplNotFound))
          }
        }
      }

      "fail if vacation not found" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val vacIn = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 10))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 9, 5), until = LocalDate.of(2018, 9, 12))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac <- vacService.createVac(empl.employeeId, vacIn).value.map(_.right.get)

            result <- vacService.updateVac(empl.employeeId, 2, updVacIn).value
            created <- vacRepo.getVacs(empl.employeeId, VacsQryParams()).value
          } yield {
            created shouldEqual Right(List(vac))
            result shouldEqual Left(AppError.ServiceValidationErrWrapper(VacNotFound))
          }
        }
      }
    }

    "update past or current vacation" should {

      "return error" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn = VacationIn(since = LocalDate.of(2018, 3, 1), until = LocalDate.of(2018, 3, 10))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 9, 5), until = LocalDate.of(2018, 9, 12))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac <- vacRepo.createVac(empl.employeeId, vacIn).value.map(_.right.get)

            result <- vacService.updateVac(empl.employeeId, vac.vacationId, updVacIn).value
            created <- vacRepo.getVac(empl.employeeId, vac.vacationId).value
          } yield {
            created shouldEqual Right(Some(vac))
            result shouldEqual Left(AppError.ServiceValidationErrWrapper(CannotChangeOrDeleteNotFutureVac))
          }
        }
      }
    }

    "not valid vacation input" should {
      "fail on: too many employees of one position will on vac in the same time" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val emplIn3 = EmployeeIn(firstName = "Frank", lastName = "Cassady", positionId = 1)
          val emplIn4 = EmployeeIn(firstName = "Jim", lastName = "Burton", positionId = 1)
          val emplIn5 = EmployeeIn(firstName = "Malcolm", lastName = "Montgomery", positionId = 1)

          val emplIn6 = EmployeeIn(firstName = "Walter", lastName = "White", positionId = 2)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 20), until = LocalDate.of(2018, 7, 25))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 15), until = LocalDate.of(2018, 7, 22))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 9, 5), until = LocalDate.of(2018, 9, 15))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 7, 18), until = LocalDate.of(2018, 7, 25))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            empl2 <- emplRepo.createEmpl(emplIn2).value.map(_.right.get)
            empl3 <- emplRepo.createEmpl(emplIn3).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn4).value
            _ <- emplRepo.createEmpl(emplIn5).value

            empl6 <- emplRepo.createEmpl(emplIn6).value.map(_.right.get)

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1).value.map(_.right.get)
            vac2 <- vacService.createVac(empl2.employeeId, vacIn2).value.map(_.right.get)
            vac3 <- vacService.createVac(empl3.employeeId, vacIn3).value.map(_.right.get)
            
            _ <- vacService.createVac(empl6.employeeId, vacIn3).value

            result <- vacService.updateVac(empl3.employeeId, vac3.vacationId, updVacIn).value
            created <- vacRepo.getVac(empl3.employeeId, vac3.vacationId).value
          } yield {
            created shouldEqual Right(Some(vac3))
            result shouldEqual Left(AppError.ServiceValidationErrsWrapper(NonEmptyList.one(TooManyEmplsOfOnePosOnVac)))
          }
        }
      }

      "fail on: not enough days between prev vacation & new vacation and between new vacation & next vacation" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 7, 21), until = LocalDate.of(2018, 7, 27))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 7, 8), until = LocalDate.of(2018, 7, 20))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacService.createVac(empl1.employeeId, vacIn1).value.map(_.right.get)
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2).value.map(_.right.get)
            _ <- vacService.createVac(empl1.employeeId, vacIn3).value.map(_.right.get)

            result <- vacService.updateVac(empl1.employeeId, vac2.vacationId, updVacIn).value
            created <- vacRepo.getVac(empl1.employeeId, vac2.vacationId).value
          } yield {
            created shouldEqual Right(Some(vac2))
            result shouldEqual Left(
              AppError.ServiceValidationErrsWrapper(NonEmptyList(NotEnoughDaysPassFromLastVac, List(NotEnoughDaysToNextVac)))
            )
          }

        }
      }

      "fail on: vacations should not overlapped" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 10))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 20), until = LocalDate.of(2018, 7, 25))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 7, 8), until = LocalDate.of(2018, 7, 15))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacService.createVac(empl1.employeeId, vacIn1).value
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2).value.map(_.right.get)

            result <- vacService.updateVac(empl1.employeeId, vac2.vacationId, updVacIn).value
            created <- vacRepo.getVac(empl1.employeeId, vac2.vacationId).value
          } yield {
            created shouldEqual Right(Some(vac2))
            result shouldEqual Left(
              AppError.ServiceValidationErrsWrapper(NonEmptyList.one(VacsMustNotOverlap))
            )
          }

        }
      }

      "fail on: max count vacation days per year exceeded" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 16))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 8, 1), until = LocalDate.of(2018, 8, 6))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 8, 1), until = LocalDate.of(2018, 8, 16))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacService.createVac(empl1.employeeId, vacIn1).value
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2).value.map(_.right.get)

            result <- vacService.updateVac(empl1.employeeId, vac2.vacationId, updVacIn).value
            created <- vacRepo.getVac(empl1.employeeId, vac2.vacationId).value
          } yield {
            created shouldEqual Right(Some(vac2))
            result shouldEqual Left(
              AppError.ServiceValidationErrsWrapper(NonEmptyList.one(MaxCountVacDaysPerYearExceeded))
            )
          }

        }
      }
    }

    "valid vacation input" should {

      "update" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 7, 21), until = LocalDate.of(2018, 7, 27))
          val updVacIn = VacationIn(since = LocalDate.of(2018, 8, 8), until = LocalDate.of(2018, 8, 20))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacService.createVac(empl1.employeeId, vacIn1).value
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2).value.map(_.right.get)
            _ <- vacService.createVac(empl1.employeeId, vacIn3).value

            result <- vacService.updateVac(empl1.employeeId, vac2.vacationId, updVacIn).value
            updated = result.right.get
          } yield {
            updated.since shouldEqual updVacIn.since
            updated.until shouldEqual updVacIn.until
          }
        }
      }
    }
  }

  "Delete vacation" when {
    "vacation not found" should {

      "return error: vacation not found" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)
          val vacIn = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 10))
          
          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac <- vacService.createVac(empl.employeeId, vacIn).value.map(_.right.get)

            result <- vacService.deleteVac(empl.employeeId, 2).value
          } yield {
            result shouldEqual Left(AppError.ServiceValidationErrWrapper(VacNotFound))
          }
        }
      }

    }

    "delete past or current vacation" should {

      "return error" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn = VacationIn(since = LocalDate.of(2018, 3, 1), until = LocalDate.of(2018, 3, 10))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac <- vacRepo.createVac(empl.employeeId, vacIn).value.map(_.right.get)

            result <- vacService.deleteVac(empl.employeeId, vac.vacationId).value
            created <- vacRepo.getVac(empl.employeeId, vac.vacationId).value
          } yield {
            created shouldEqual Right(Some(vac))
            result shouldEqual Left(AppError.ServiceValidationErrWrapper(CannotChangeOrDeleteNotFutureVac))
          }

        }
      }
    }

    "delete future vacation" should {

      "delete" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn = VacationIn(since = LocalDate.of(2018, 6, 1), until = LocalDate.of(2018, 6, 10))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac <- vacRepo.createVac(empl.employeeId, vacIn).value.map(_.right.get)

            result <- vacService.deleteVac(empl.employeeId, vac.vacationId).value
            created <- vacRepo.getVac(empl.employeeId, vac.vacationId).value
          } yield {
            created shouldEqual Right(None)
            result shouldEqual Right(vac)
          }
        }
      }
    }

  }

  "Get vacation" when {
    "Vacation exist" should {
      "return vacation" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn = VacationIn(since = LocalDate.of(2018, 6, 1), until = LocalDate.of(2018, 6, 10))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac <- vacRepo.createVac(empl.employeeId, vacIn).value.map(_.right.get)

            result <- vacService.getVac(empl.employeeId, vac.vacationId).value
          } yield {
            result shouldEqual Right(Some(vac))
          }
        }
      }
    }

    "Vacation not exist" should {
      "return None" in withCtx {
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Sam", lastName = "Watson", positionId = 1)

          val vacIn = VacationIn(since = LocalDate.of(2018, 6, 1), until = LocalDate.of(2018, 6, 10))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacRepo.createVac(empl.employeeId, vacIn).value

            result <- vacService.getVac(empl.employeeId, 2).value
          } yield {
            result shouldEqual Right(None)
          }
        }
      }
    }
  }

  "Get vacations by query params" when {

    "with empty query params and not existing employee" should {
      
      "return error" in withCtx { 
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          for {
            result <- vacService.getVacs(1, VacsQryParams()).value
          } yield {
            result shouldEqual Left(AppError.ServiceValidationErrWrapper(EmplNotFound))
          }
        }
      }
    }

    "with empty query params and not existing vacations" should {
      
      "return empty list of vacations" in withCtx { 
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)

          for {
            empl <- emplRepo.createEmpl(emplIn).value.map(_.right.get)
            
            result <- vacService.getVacs(empl.employeeId, VacsQryParams()).value
          } yield {
            result shouldEqual Right(List())
          }
        }
      }
    }

    "with query params and existing vacations" should {
      
      "return list of vacations in asc order by vacation_id" in withCtx { 
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 7, 21), until = LocalDate.of(2018, 7, 27))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl.employeeId, vacIn1).value.map(_.right.get)
            vac2 <- vacService.createVac(empl.employeeId, vacIn2).value.map(_.right.get)
            vac3 <- vacService.createVac(empl.employeeId, vacIn3).value.map(_.right.get)

            result <- vacService.getVacs(empl.employeeId, VacsQryParams()).value
          } yield {
            result shouldEqual Right(List(vac1, vac2, vac3))
          }
        }
      }
    }

    "with empty query params and existing vacations" should {
      
      "return list of vacations in desc order by since date" in withCtx { 
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 8, 21), until = LocalDate.of(2018, 8, 27))

          val qryParams = VacsQryParams(orderByParams = Some(OrderByParams(field = "since", asc = false)))

          for {
            empl <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl.employeeId, vacIn1).value.map(_.right.get)
            vac2 <- vacService.createVac(empl.employeeId, vacIn2).value.map(_.right.get)
            vac3 <- vacService.createVac(empl.employeeId, vacIn3).value.map(_.right.get)

            result <- vacService.getVacs(empl.employeeId, qryParams).value
          } yield {
            result shouldEqual Right(List(vac3, vac2, vac1))
          }
          
        }
      }

      "return filterd list of vacations in asc order by since date" in withCtx { 
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 8, 21), until = LocalDate.of(2018, 8, 27))

          val qryParams = VacsQryParams(orderByParams = Some(OrderByParams(field = "since", asc = true)),
                                        since = Some(DateParams(before = Some(LocalDate.of(2018, 8, 21)))))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            vac1 <- vacService.createVac(empl1.employeeId, vacIn1).value.map(_.right.get)
            vac2 <- vacService.createVac(empl1.employeeId, vacIn2).value.map(_.right.get)
            _ <- vacService.createVac(empl1.employeeId, vacIn3).value

            result <- vacService.getVacs(empl1.employeeId, qryParams).value
          } yield {
            result shouldEqual Right(List(vac1, vac2))
          }
          
        }
      }

      "return filterd list" in withCtx { 
        (emplRepo, vacRepo) => {
          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val serviceValidation = new ServiceValidationInterpreter(clock)
          val vacService = new VacationService[IO](vacRepo, emplRepo, serviceValidation)

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Jack", lastName = "Holmes", positionId = 1)

          val vacIn1 = VacationIn(since = LocalDate.of(2018, 7, 1), until = LocalDate.of(2018, 7, 6))
          val vacIn2 = VacationIn(since = LocalDate.of(2018, 7, 11), until = LocalDate.of(2018, 7, 16))
          val vacIn3 = VacationIn(since = LocalDate.of(2018, 8, 21), until = LocalDate.of(2018, 8, 27))

          val qryParams = VacsQryParams(since = Some(DateParams(after = Some(LocalDate.of(2018, 8, 20)), 
                                                                exact = Some(LocalDate.of(2018, 8, 21)))))

          for {
            empl1 <- emplRepo.createEmpl(emplIn1).value.map(_.right.get)
            _ <- emplRepo.createEmpl(emplIn2).value

            _ <- vacService.createVac(empl1.employeeId, vacIn1).value
            _ <- vacService.createVac(empl1.employeeId, vacIn2).value
            vac3 <- vacService.createVac(empl1.employeeId, vacIn3).value.map(_.right.get)

            result <- vacService.getVacs(empl1.employeeId, qryParams).value
          } yield {
            result shouldEqual Right(List(vac3))
          }
          
        }
      }
    }

  }
  
}