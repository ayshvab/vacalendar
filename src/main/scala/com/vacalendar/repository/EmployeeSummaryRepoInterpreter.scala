package com.vacalendar.repository

import java.time.LocalDate

import cats._
import cats.implicits._
import cats.data._
import doobie._
import doobie.implicits._

import com.vacalendar.endpoints.QryParams._
import com.vacalendar.domain._
import com.vacalendar.errors._

class EmployeeSummaryRepoInterpreter[F[_]](val xa: Transactor[F])
                                          (implicit F: MonadError[F, Throwable]) extends EmployeeSummaryRepoAlgebra[F] {

  def getEmplSummary(emplId: Long): EitherT[F, AppError, Option[EmployeeSummary]] = {
    val program: ConnectionIO[EmployeeSummary] = for {
      empl <- EmployeeSQL.selectEmpl(emplId).unique
      pos <- PositionSQL.selectPos(empl.positionId).unique
      vacs <- VacationSQL.selectEmplVacsCurrYear(empl.employeeId).list
    } yield EmployeeSummary(empl, pos, vacs)

    program.map(Option.apply)
      .transact(xa)
      .attemptT 
      .leftMap[AppError](AppError.DbErrWrapper)
  }

  def getEmplSummaries(qryParams: EmplSummariesQryParams = EmplSummariesQryParams()): EitherT[F, AppError, List[EmployeeSummary]] = {
    val program: ConnectionIO[List[EmployeeSummary]] = for {
      empls <- EmployeeSQL.selectEmpls().list
      emplSumrs <- empls.reverse.traverse { empl =>
        for {
          pos <- PositionSQL.selectPos(empl.positionId).unique
          vacs <- VacationSQL.selectEmplVacsCurrYear(empl.employeeId).list
        } yield EmployeeSummary(empl, pos, vacs)
      }
    } yield filterAndOrderEmplSumrs(emplSumrs, qryParams) 

    program
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)
  }

  private def filterAndOrderEmplSumrs(emplsSumrs: List[EmployeeSummary], qryParams: EmplSummariesQryParams): List[EmployeeSummary] = {
    val go = (dp: DateParams, vd: LocalDate) => 
      dp.before.map(b => vd.isBefore(b)).getOrElse(true) &
      dp.exact.map(e => vd.isEqual(e)).getOrElse(true) &
      dp.after.map(a => vd.isAfter(a)).getOrElse(true)
    
    val filters = Seq(
      qryParams.employeeId.map(emplId => (sumr: EmployeeSummary) => sumr.employeeId == emplId),

      qryParams.firstName.map(fn => (sumr: EmployeeSummary) => sumr.firstName == fn),
      qryParams.lastName.map(ln => (sumr: EmployeeSummary) => sumr.lastName == ln),

      qryParams.positionId.map(posId => (sumr: EmployeeSummary) => sumr.positionId == posId),
      qryParams.positionTitle.map(posTitle => (sumr: EmployeeSummary) => sumr.positionTitle == posTitle),

      qryParams.isOnVac.map(is => (sumr: EmployeeSummary) => sumr.isOnVacation == is),

      qryParams.pastVacsSince.map(dateParams => 
        (sumr: EmployeeSummary) => sumr.pastVacations.exists((vac: Vacation) => go(dateParams, vac.since))),
      qryParams.pastVacsUntil.map(dateParams => 
        (sumr: EmployeeSummary) => sumr.pastVacations.exists((vac: Vacation) => go(dateParams, vac.until))),

      qryParams.currVacSince.map(dateParams => 
        (sumr: EmployeeSummary) => 
          sumr.currentVacation.map((vac: Vacation) => go(dateParams, vac.since)).getOrElse(true)),
      qryParams.currVacUntil.map(dateParams => 
        (sumr: EmployeeSummary) => 
          sumr.currentVacation.map((vac: Vacation) => go(dateParams, vac.until)).getOrElse(true)),

      qryParams.futureVacsSince.map(dateParams => 
        (sumr: EmployeeSummary) => sumr.futureVacations.exists((vac: Vacation) => go(dateParams, vac.since))),
      qryParams.futureVacsUntil.map(dateParams => 
        (sumr: EmployeeSummary) => sumr.futureVacations.exists((vac: Vacation) => go(dateParams, vac.until)))
    )

    val actualFilters = filters.filter(_.isDefined).map(_.get)

    val defaultOrderBy = (sumr1: EmployeeSummary, sumr2: EmployeeSummary) => 
        sumr1.employeeId.compareTo(sumr2.employeeId) < 0

    val orderBy = qryParams.orderByParams match {
      case None => defaultOrderBy

      case Some(oBy) => oBy match {

        case (OrderByParams("employeeId", asc)) => 
          (sumr1: EmployeeSummary, sumr2: EmployeeSummary) => 
            asc == sumr1.employeeId.compareTo(sumr2.employeeId) < 0

        case OrderByParams("firstName", asc) => 
          (sumr1: EmployeeSummary, sumr2: EmployeeSummary) =>
            asc == sumr1.firstName.compareTo(sumr2.firstName) < 0

        case OrderByParams("lastName", asc) => 
          (sumr1: EmployeeSummary, sumr2: EmployeeSummary) =>
            asc == sumr1.lastName.compareTo(sumr2.lastName) < 0

        case OrderByParams("positionId", asc) => 
          (sumr1: EmployeeSummary, sumr2: EmployeeSummary) =>
            asc == sumr1.positionId.compareTo(sumr2.positionId) < 0

        case OrderByParams("remainedVacationDays", asc) => 
          (sumr1: EmployeeSummary, sumr2: EmployeeSummary) =>
            asc == sumr1.remainedVacationDaysCount.compareTo(sumr2.remainedVacationDaysCount) < 0

        case _ => defaultOrderBy
        }
    }

    emplsSumrs
      .filter(sumr => actualFilters.forall(filter => filter(sumr)))
      .sortWith(orderBy)
  }

}