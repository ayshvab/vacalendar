package com.vacalendar.repository

import java.time.{ Instant, LocalDate, Year, Clock }

import cats._
import cats.implicits._
import cats.data._
import doobie._
import doobie.implicits._
import Fragments._

import com.vacalendar.endpoints.QryParams._
import com.vacalendar.domain._
import com.vacalendar.errors._

object VacationSQL {
  def selectVac(emplId: Long, vacId: Long): Query0[Vacation] =
    sql"""
      SELECT *
      FROM vacations
      WHERE employee_id = $emplId AND vacation_id = $vacId
    """
    .query[Vacation]

  def selectVacs(emplId: Long, qryParams: VacsQryParams = VacsQryParams()): Query0[Vacation] = {
    val whereEmployeeId = Some(fr"employee_id = $emplId")

    val whereSinceBefore = qryParams.since.flatMap(_.before.map(ld => fr"since < $ld"))
    val whereSince = qryParams.since.flatMap(_.exact.map(ld => fr"since = $ld"))
    val whereSinceAfter = qryParams.since.flatMap(_.after.map(ld => fr"since > $ld"))

    val whereUntilBefore = qryParams.until.flatMap(_.before.map(ld => fr"until < $ld"))
    val whereUntil = qryParams.until.flatMap(_.exact.map(ld => fr"until = $ld"))
    val whereUntilAfter = qryParams.until.flatMap(_.after.map(ld => fr"until > $ld"))

    val defaultOrderBy: Fragment = fr"order by vacation_id desc"

    val optOrderBy: Option[Fragment] = qryParams.orderByParams map { o => 
      if (o.asc) fr"order by" ++ Fragment.const(o.field) ++ fr"asc"
      else fr"order by" ++ Fragment.const(o.field) ++ fr"desc"
    }

    val orderBy = optOrderBy getOrElse(defaultOrderBy)

    val q: Fragment = 
      fr"SELECT * FROM vacations" ++ 
      whereAndOpt(whereEmployeeId, 
                  whereSinceBefore,
                  whereSince,
                  whereSinceAfter,
                  whereUntilBefore,
                  whereUntil,
                  whereUntilAfter) ++
      orderBy
 
    q.query[Vacation]

  }

  def deleteVac(emplId: Long, vacId: Long): Query0[Vacation] =
    sql"""
    DELETE FROM vacations
    WHERE employee_id = $emplId AND vacation_id = $vacId
    RETURNING *;
    """
    .query[Vacation]

  def insertVac(emplId: Long, vacIn: VacationIn, clock: Clock): Query0[Vacation] =
    sql"""
      INSERT INTO vacations (employee_id, since, until, created)
      VALUES (${emplId}, ${vacIn.since}, ${vacIn.until}, ${Instant.now(clock)})
      RETURNING *
    """
    .query[Vacation]

  def updateVac(emplId: Long, vacId: Long, vacIn: VacationIn, clock: Clock): Query0[Vacation] =
    sql"""
      UPDATE vacations
      SET
        employee_id = ${emplId},
        since = ${vacIn.since},
        until = ${vacIn.until},
        updated = ${Instant.now(clock)}
      WHERE vacation_id = $vacId
      RETURNING *
    """
    .query[Vacation]

  def selectEmplVacsCurrYear(emplId: Long, clock: Clock): Query0[Vacation] = {
    val currY = Year.now(clock)
    val startYear = LocalDate.of(currY.getValue, 1, 1)
    val endYear = LocalDate.of(currY.getValue, 12, 31)
    sql"""
      SELECT *
      FROM vacations
      WHERE (since, until) OVERLAPS ($startYear, $endYear) AND employee_id = $emplId
      ORDER BY since
    """
    .query[Vacation]
  }

  def selectOverlappedPosIdVacsCurrYear(posId: Long,
                                       vacStart: LocalDate,
                                       vacEnd: LocalDate,
                                       clock: Clock): Query0[Vacation] = {

      val currYear = Year.now(clock)
      val startYear = LocalDate.of(currYear.getValue, 1, 1)
      val endYear = LocalDate.of(currYear.getValue, 12, 31)

      sql"""
        SELECT
          vacations.*
        FROM
          vacations
        INNER JOIN (
          SELECT *
          FROM employees
          WHERE employees.position_id = $posId
        ) employees_with_target_position_id
        ON vacations.employee_id = employees_with_target_position_id.employee_id
        WHERE (vacations.since, vacations.until) OVERLAPS ($startYear, $endYear) AND
          (vacations.since, vacations.until) OVERLAPS ($vacStart, $vacEnd)
        ORDER BY vacations.since
      """
      .query[Vacation]
    }
}

class VacationRepoInterpreter[F[_]](val xa: Transactor[F])
                                   (implicit F: MonadError[F, Throwable]) extends VacationRepoAlgebra[F] {


  def getVac(emplId: Long, vacId: Long): EitherT[F, AppError, Option[Vacation]] =
    VacationSQL.selectVac(emplId, vacId).option
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)

  def getVacs(emplId: Long, qryParams: VacsQryParams): EitherT[F, AppError, List[Vacation]] =
    VacationSQL.selectVacs(emplId, qryParams).to[List]
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)

  def getEmplVacsCurrYear(emplId: Long, clock: Clock = Clock.systemUTC()): EitherT[F, AppError, List[Vacation]] = {
    VacationSQL.selectEmplVacsCurrYear(emplId, clock).to[List]
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)
  }

  def getOverlappedPosIdVacs(posId: Long,
                             startVac: LocalDate,
                             endVac: LocalDate,
                             clock: Clock = Clock.systemUTC()): EitherT[F, AppError, List[Vacation]] = {
    VacationSQL.selectOverlappedPosIdVacsCurrYear(posId, startVac, endVac, clock)
      .to[List]
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)
  }

  def deleteVac(emplId: Long, vacId: Long): EitherT[F, AppError, Option[Vacation]] =
    VacationSQL.deleteVac(emplId, vacId).option
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)

  def createVac(emplId: Long, 
                vacIn: VacationIn,
                clock: Clock = Clock.systemUTC()): EitherT[F, AppError, Vacation] =
    VacationSQL.insertVac(emplId, vacIn, clock).unique
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)

  def updateVac(emplId: Long, 
                vacId: Long, 
                vacIn: VacationIn, 
                clock: Clock = Clock.systemUTC()): EitherT[F, AppError, Option[Vacation]] =
    VacationSQL.updateVac(emplId, vacId, vacIn, clock).option
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)
}