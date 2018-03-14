package com.test.vacalendar.service

import java.time._
import cats.effect._
import org.scalatest._
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import doobie.hikari.implicits._

import com.vacalendar.conf.DatabaseConfig
import com.vacalendar.Module
import com.vacalendar.domain._
import com.vacalendar.errors._
import com.vacalendar.endpoints.QryParams._
import com.vacalendar.validation.ValidationRules
import com.vacalendar.repository._
import com.vacalendar.service.EmployeeSummaryService

class EmployeeSummaryServiceSpec extends WordSpec
                                 with Matchers
                                 with ForAllTestContainer {

  override val container = PostgreSQLContainer()

  def withCtx(testThunk: (EmployeeRepoInterpreter[IO], 
                          VacationRepoInterpreter[IO], 
                          EmployeeSummaryRepoInterpreter[IO], 
                          EmployeeSummaryService[IO]) => IO[Unit]): Unit = {

    val dbConf = DatabaseConfig(driver = "org.postgresql.Driver",
                                url = container.jdbcUrl,
                                user = container.username,
                                password = container.password)

    val m = new Module[IO]()
    val xa = m.dbTransactor(dbConf).unsafeRunSync()

    val emplRepo = new EmployeeRepoInterpreter[IO](xa)
    val vacRepo = new VacationRepoInterpreter[IO](xa)
    val emplSummaryRepo = new EmployeeSummaryRepoInterpreter[IO](xa)

    val emplSummaryService = new EmployeeSummaryService[IO](emplSummaryRepo)

    m.migrateDb(xa).unsafeRunSync()

    try {
      testThunk(emplRepo, vacRepo, emplSummaryRepo, emplSummaryService).unsafeRunSync()
    } finally {
      m.dropDb(xa).unsafeRunSync()
      xa.shutdown.unsafeRunSync()
    }
  }

  "Get employee summary" when {

    "employee with employeeId not exist" should {

      "return error Employee Not Found" in withCtx {
        (emplRepo, vacRepo, emplSummaryRepo, emplSummaryService) => {

          for {
            result <- emplSummaryService.getEmplSummary(1).value
          } yield {
            result shouldEqual Left(AppError.ServiceValidationErrWrapper(EmplNotFound))
          }
        }
      }
    }

    "employee with employeeId exist" should {

      "return employee summary 1" in withCtx {
        (emplRepo, vacRepo, emplSummaryRepo, emplSummaryService) => {

          val emplId: Long = 1

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)

          emplRepo.createEmpl(emplIn1).value.unsafeRunSync()
          emplRepo.createEmpl(emplIn2).value.unsafeRunSync()

          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val currVacIn = VacationIn(since = LocalDate.now(clock), until = LocalDate.now(clock).plusDays(5))
          val currVacDays = currVacIn.until.toEpochDay - currVacIn.since.toEpochDay
          val pastVacIn = VacationIn(since = LocalDate.of(2018, 2, 1), until = LocalDate.of(2018, 2, 8))
          val pastVacDays = pastVacIn.until.toEpochDay - pastVacIn.since.toEpochDay
          val futureVacIn = VacationIn(since = LocalDate.of(2018, 8, 1), until = LocalDate.of(2018, 8, 10))
          val futureVacDays = futureVacIn.until.toEpochDay - futureVacIn.since.toEpochDay

          val clockOfVacsCreation = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val currVac = vacRepo.createVac(emplId, currVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()
          val pastVac = vacRepo.createVac(emplId, pastVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()
          val futureVac = vacRepo.createVac(emplId, futureVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()

          for {
            result <- emplSummaryService.getEmplSummary(emplId, clock).value
            summary = result.right.get
          } yield {
            summary.employeeId shouldEqual emplId
            summary.firstName shouldEqual emplIn1.firstName
            summary.lastName shouldEqual emplIn1.lastName
            summary.positionId shouldEqual emplIn1.positionId
            summary.positionId shouldEqual emplIn1.positionId
            summary.remainedVacationDaysCount shouldEqual (ValidationRules.vacDaysMaxCountPerYear - (currVacDays + pastVacDays + futureVacDays))
            summary.isOnVacation shouldEqual true
            summary.pastVacations shouldEqual List(pastVac)
            summary.currentVacation shouldEqual Some(currVac)
            summary.futureVacations shouldEqual List(futureVac)
          }
        }
      }
    }
  }

  "Get employee summaries by query params" when {
    
    "with empty qryParams and prepared db" should {
      "return list of employee summaries in default order" in withCtx {
        (emplRepo, vacRepo, emplSummaryRepo, emplSummaryService) => {
          val emplId1: Long = 1
          val emplId2: Long = 2

          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)

          emplRepo.createEmpl(emplIn1).value.unsafeRunSync()
          emplRepo.createEmpl(emplIn2).value.unsafeRunSync()

          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

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

          val clockOfVacsCreation = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val empl1CurrVac = vacRepo.createVac(emplId1, empl1CurrVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()
          val empl1PastVac = vacRepo.createVac(emplId1, empl1PastVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()
          val empl1FutureVac = vacRepo.createVac(emplId1, empl1FutureVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()

          val empl2PastVac1 = vacRepo.createVac(emplId2, empl2PastVacIn1, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()
          val empl2PastVac2 = vacRepo.createVac(emplId2, empl2PastVacIn2, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()
          val empl2FutureVac = vacRepo.createVac(emplId2, empl2FutureVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()

          for {
            result <- emplSummaryService.getEmplSummaries(EmplSummariesQryParams(), clock).value
            summaries = result.right.get
            expected1 = EmployeeSummary(employeeId = emplId1,
                                        firstName = emplIn1.firstName,
                                        lastName = emplIn1.lastName,
                                        positionId = emplIn1.positionId,
                                        positionTitle = "developer",
                                        remainedVacationDaysCount = empl1RemainedVacationDaysCount,
                                        isOnVacation = true,
                                        pastVacations = List(empl1PastVac),
                                        currentVacation = Some(empl1CurrVac),
                                        futureVacations = List(empl1FutureVac))

            expected2 = EmployeeSummary(employeeId = emplId2,
                                        firstName = emplIn2.firstName,
                                        lastName = emplIn2.lastName,
                                        positionId = emplIn2.positionId,
                                        positionTitle = "developer",
                                        remainedVacationDaysCount = empl2RemainedVacationDaysCount,
                                        isOnVacation = false,
                                        pastVacations = List(empl2PastVac2, empl2PastVac1),
                                        currentVacation = None,
                                        futureVacations = List(empl2FutureVac))
          } yield {
            summaries shouldEqual List(expected1, expected2)
          }
        }
      }        
    }

    "with qryParams and prepared db" should {
      "return ordered and filterd list of employee summaries 1" in withCtx {
        (emplRepo, vacRepo, emplSummaryRepo, emplSummaryService) => {
          val emplId1: Long = 1
          val emplId2: Long = 2
          val emplId3: Long = 3
          
          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn3 = EmployeeIn(firstName = "Montgomery", lastName = "Montgomery", positionId = 2)

          emplRepo.createEmpl(emplIn1).value.unsafeRunSync()
          emplRepo.createEmpl(emplIn2).value.unsafeRunSync()
          emplRepo.createEmpl(emplIn3).value.unsafeRunSync()

          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

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

          val clockOfVacsCreation = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          vacRepo.createVac(emplId1, empl1CurrVacIn, clockOfVacsCreation)
            .value.unsafeRunSync()
          vacRepo.createVac(emplId1, empl1PastVacIn, clockOfVacsCreation)
            .value.unsafeRunSync()
          vacRepo.createVac(emplId1, empl1FutureVacIn, clockOfVacsCreation)
            .value.unsafeRunSync()

          val empl2PastVac1 = vacRepo.createVac(emplId2, empl2PastVacIn1, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()
          val empl2PastVac2 = vacRepo.createVac(emplId2, empl2PastVacIn2, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()
          val empl2FutureVac = vacRepo.createVac(emplId2, empl2FutureVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()

          val qryParams = EmplSummariesQryParams(orderByParams = Some(OrderByParams(field = "remainedVacationDays", asc = true)),
                                                 isOnVac = Some(false))

          for {
            result <- emplSummaryService.getEmplSummaries(qryParams, clock).value
            summaries = result.right.get
            expected1 = EmployeeSummary(employeeId = emplId2,
                                        firstName = emplIn2.firstName,
                                        lastName = emplIn2.lastName,
                                        positionId = emplIn2.positionId,
                                        positionTitle = "developer",
                                        remainedVacationDaysCount = empl2RemainedVacationDaysCount,
                                        isOnVacation = false,
                                        pastVacations = List(empl2PastVac2, empl2PastVac1),
                                        currentVacation = None,
                                        futureVacations = List(empl2FutureVac))

            expected2 = EmployeeSummary(employeeId = emplId3,
                                        firstName = emplIn3.firstName,
                                        lastName = emplIn3.lastName,
                                        positionId = emplIn3.positionId,
                                        positionTitle = "designer",
                                        remainedVacationDaysCount = ValidationRules.vacDaysMaxCountPerYear.toLong,
                                        isOnVacation = false,
                                        pastVacations = List(),
                                        currentVacation = None,
                                        futureVacations = List())
          } yield {
            summaries shouldEqual List(expected1, expected2)
          }
        }
      }        
    }

    "return ordered and filterd list of employee summaries 2" in withCtx {
        (emplRepo, vacRepo, emplSummaryRepo, emplSummaryService) => {
          val emplId1: Long = 1
          val emplId2: Long = 2
          
          val emplIn1 = EmployeeIn(firstName = "John", lastName = "Doe", positionId = 1)
          val emplIn2 = EmployeeIn(firstName = "Peter", lastName = "Parker", positionId = 1)
          val emplIn3 = EmployeeIn(firstName = "Montgomery", lastName = "Montgomery", positionId = 2)

          emplRepo.createEmpl(emplIn1).value.unsafeRunSync()
          emplRepo.createEmpl(emplIn2).value.unsafeRunSync()
          emplRepo.createEmpl(emplIn3).value.unsafeRunSync()

          val clock = Clock.fixed(LocalDate.of(2018, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

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
          
          val clockOfVacsCreation = Clock.fixed(LocalDate.of(2018, 1, 1).atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

          val empl1CurrVac = vacRepo.createVac(emplId1, empl1CurrVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()
          val empl1PastVac = vacRepo.createVac(emplId1, empl1PastVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()
          val empl1FutureVac = vacRepo.createVac(emplId1, empl1FutureVacIn, clockOfVacsCreation)
            .value.map(_.right.get).unsafeRunSync()

          vacRepo.createVac(emplId2, empl2PastVacIn1, clockOfVacsCreation)
            .value.unsafeRunSync()
          vacRepo.createVac(emplId2, empl2PastVacIn2, clockOfVacsCreation)
            .value.unsafeRunSync()
          vacRepo.createVac(emplId2, empl2FutureVacIn, clockOfVacsCreation)
            .value.unsafeRunSync()

          val qryParams = EmplSummariesQryParams(orderByParams = Some(OrderByParams(field = "remainedVacationDays", asc = false)),
                                                 pastVacsSince = Some(DateParams(before = Some(LocalDate.of(2018, 6, 1)), after = Some(LocalDate.of(2018, 1, 31)))),
                                                 futureVacsUntil = Some(DateParams(exact = Some(LocalDate.of(2018, 8, 10)))))

          for {
            result <- emplSummaryService.getEmplSummaries(qryParams, clock).value
            summaries = result.right.get
            expected = EmployeeSummary(employeeId = emplId1,
                                       firstName = emplIn1.firstName,
                                       lastName = emplIn1.lastName,
                                       positionId = emplIn1.positionId,
                                       positionTitle = "developer",
                                       remainedVacationDaysCount = empl1RemainedVacationDaysCount,
                                       isOnVacation = true,
                                       pastVacations = List(empl1PastVac),
                                       currentVacation = Some(empl1CurrVac),
                                       futureVacations = List(empl1FutureVac))
          
          } yield {
            summaries shouldEqual List(expected)
          }
        }
      }        
    }

}