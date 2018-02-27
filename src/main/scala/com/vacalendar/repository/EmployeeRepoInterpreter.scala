package com.vacalendar.repository

import java.time.{Instant, LocalDate, Year}

import cats._
import cats.MonadError
import cats.implicits._
import doobie._
import doobie.implicits._
import Fragments._
import doobie.util.invariant.UnexpectedEnd
import com.vacalendar.domain.employees._

import com.vacalendar.endpoint.EmplsQryParams
import com.vacalendar.endpoint.VacsQryParams
import com.vacalendar.endpoint.SumrsQryParams
import com.vacalendar.endpoint.DateParams
import com.vacalendar.endpoint.OrderByParams

import com.vacalendar.domain.vacations.{Vacation, VacationIn}
import com.vacalendar.domain.positions.Position

object EmployeeSQL {

  implicit val InstantMeta: Meta[Instant] =
    Meta[java.sql.Timestamp].xmap(
      ts => ts.toInstant,
      inst => java.sql.Timestamp.from(inst)
    )

  implicit val LocalDateMeta: Meta[LocalDate] =
    Meta[java.sql.Date].xmap(
      sqlDate => sqlDate.toLocalDate,
      ld => java.sql.Date.valueOf(ld)
    )

  def insertEmployee(employeeIn: EmployeeIn): Query0[Employee] =
    sql"""
      INSERT INTO employees (first_name, last_name, position_id)
      VALUES (${employeeIn.firstName}, ${employeeIn.lastName}, ${employeeIn.positionId})
      RETURNING *
    """.query[Employee]

  def updateEmployee(employeeId: Long, employeeIn: EmployeeIn): Query0[Employee] =
    sql"""
      UPDATE employees
      SET
        first_name = ${employeeIn.firstName},
        last_name = ${employeeIn.lastName},
        position_id = ${employeeIn.positionId},
        updated = ${Instant.now()}
      WHERE employee_id = $employeeId
      RETURNING *
    """.query[Employee]

  def selectEmployee(employeeId: Long): Query0[Employee] =
    sql"""
      SELECT *
      FROM employees
      WHERE employees.employee_id = $employeeId
    """.query[Employee]

  def selectEmployees(params: EmplsQryParams = EmplsQryParams()): Query0[Employee] = {
    val whereFirstName = params.firstName.map(s => fr"first_name like $s")
    val whereLastName = params.lastName.map(s => fr"last_name like $s")
    val wherePositionId = params.positionId.map(n => fr"position_id = $n")
  
    val defaultOrderBy: Fragment = fr"order by employee_id desc"

    val optOrderBy: Option[Fragment] = params.orderByParams map { o => 
      if (o.asc) fr"order by" ++ Fragment.const(o.field) ++ fr"asc"
      else fr"order by" ++ Fragment.const(o.field) ++ fr"desc"
    }

    val orderBy = optOrderBy getOrElse(defaultOrderBy)

    val q: Fragment = fr"SELECT * FROM employees" ++ 
      whereAndOpt(whereFirstName, whereLastName, wherePositionId) ++
      orderBy
 
    q.query[Employee]
  }

  def deleteEmployee(employeeId: Long) : Query0[Employee] =
    sql"""
    DELETE FROM employees
    WHERE employee_id = $employeeId
    RETURNING *
    """
    .query[Employee]

  def selectVac(employeeId: Long, vacId: Long): Query0[Vacation] =
    sql"""
      SELECT vacation_id, employee_id, since, until, created, updated
      FROM vacations
      WHERE employee_id = $employeeId AND vacation_id = $vacId
    """
    .query[Vacation]

  def selectVacs(employeeId: Long, qryParams: VacsQryParams = VacsQryParams()): Query0[Vacation] = {
    val whereEmployeeId = Some(fr"employee_id = $employeeId")

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

  def deleteVac(employeeId: Long, vacId: Long): Query0[Vacation] =
    sql"""
    DELETE FROM vacations
    WHERE employee_id = $employeeId AND vacation_id = $vacId
    RETURNING *;
    """
    .query[Vacation]

  def insertVac(emplId: Long, vacIn: VacationIn): Query0[Vacation] =
    sql"""
      INSERT INTO vacations (employee_id, since, until)
      VALUES (${emplId}, ${vacIn.since}, ${vacIn.until})
      RETURNING *
    """
    .query[Vacation]

  def updateVac(emplId: Long, vacId: Long, vacIn: VacationIn): Query0[Vacation] =
    sql"""
      UPDATE vacations
      SET
        employee_id = ${emplId},
        since = ${vacIn.since},
        until = ${vacIn.until},
        updated = ${Instant.now()}
      WHERE vacation_id = $vacId
      RETURNING *
    """
    .query[Vacation]

  def selectEmployeeVacsForPeriod(employeeId: Long, startDate: LocalDate, endDate: LocalDate): Query0[Vacation] =
    sql"""
      SELECT vacation_id, employee_id, since, until, created, updated
      FROM vacations
      WHERE (since, until) OVERLAPS ($startDate, $endDate) AND employee_id = $employeeId
      ORDER BY since
    """
    .query[Vacation]

  def selectEmployeeVacsCurrY(employeeId: Long): Query0[Vacation] = {
    val currY = Year.now()
    val startY = LocalDate.of(currY.getValue, 1, 1)
    val endY = LocalDate.of(currY.getValue, 12, 31)
    selectEmployeeVacsForPeriod(employeeId, startY, endY)
  }

  def selectOverlappedPosIdVacsForPeriod(posId: Long,
                                         vacStart: LocalDate,
                                         vacEnd: LocalDate,
                                         startPeriod: LocalDate,
                                         endPeriod: LocalDate): Query0[Vacation] =
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
      WHERE (vacations.since, vacations.until) OVERLAPS ($startPeriod, $endPeriod) AND
       (vacations.since, vacations.until) OVERLAPS ($vacStart, $vacEnd)
      ORDER BY vacations.since
    """
    .query[Vacation]

  def selectEmployeesByPosId(posId: Long): Query0[Employee] = sql"""
      SELECT *
      FROM
        employees
      WHERE employees.position_id = $posId
      ORDER BY employees.employee_id
    """
    .query[Employee]

  def selectPosition(posId: Long): Query0[Position] = sql"""
    SELECT *
    FROM
      positions
    WHERE positions.position_id = $posId
  """
  .query[Position]

  
}

class EmployeeRepoInterpreter[F[_]](val xa: Transactor[F])
                                   (implicit F: MonadError[F, Throwable]) extends EmployeeRepoAlgebra[F] {

  // TODO maybe add Either[DatabaseError, DatabaseResult]

  def createEmployee(employeeIn: EmployeeIn): F[Employee] =
    EmployeeSQL.insertEmployee(employeeIn).unique.transact(xa)

  def updateEmployee(employeeId: Long, employeeIn: EmployeeIn): F[Option[Employee]] =
    EmployeeSQL.updateEmployee(employeeId, employeeIn).option.transact(xa)

  def listEmployees(params: EmplsQryParams): F[List[Employee]] = // ?? add Fragments
    EmployeeSQL.selectEmployees(params).list.transact(xa)

  def getEmployee(employeeId: Long): F[Option[Employee]] =
    EmployeeSQL.selectEmployee(employeeId).option.transact(xa)

  def deleteEmployee(employeeId: Long): F[Option[Employee]] =
    EmployeeSQL.deleteEmployee(employeeId).option.transact(xa)

  def getVac(employeeId: Long, vacId: Long): F[Option[Vacation]] =
    EmployeeSQL.selectVac(employeeId, vacId).option.transact(xa)

  def getVacs(employeeId: Long, qryParams: VacsQryParams): F[List[Vacation]] =
    EmployeeSQL.selectVacs(employeeId, qryParams).list.transact(xa)

  def getEmplVacsCurrYear(employeeId: Long): F[List[Vacation]] = {
    val currY = Year.now()
    val startY = LocalDate.of(currY.getValue, 1, 1)
    val endY = LocalDate.of(currY.getValue, 12, 31)
    EmployeeSQL.selectEmployeeVacsForPeriod(employeeId, startY, endY).list.transact(xa)
  }

  def getOverlappedPosIdVacs(posId: Long,
                             startVac: LocalDate,
                             endVac: LocalDate): F[List[Vacation]] = {
    val currY = Year.now()
    val startY = LocalDate.of(currY.getValue, 1, 1)
    val endY = LocalDate.of(currY.getValue, 12, 31)
    EmployeeSQL.selectOverlappedPosIdVacsForPeriod(posId, startVac, endVac, startY, endY)
      .list.transact(xa)
  }

  def getEmplsByPosId(posId: Long): F[List[Employee]] = {
    EmployeeSQL.selectEmployeesByPosId(posId).list.transact(xa)
  }

  def deleteVac(employeeId: Long, vacId: Long): F[Option[Vacation]] =
    EmployeeSQL.deleteVac(employeeId, vacId).option.transact(xa)

  def createVac(emplId: Long, vacIn: VacationIn): F[Vacation] =
    EmployeeSQL.insertVac(emplId, vacIn).unique.transact(xa)

  def updateVac(emplId: Long, vacId: Long, vacIn: VacationIn): F[Option[Vacation]] =
    EmployeeSQL.updateVac(emplId, vacId, vacIn).option.transact(xa)

  def getEmployeeSummary(employeeId: Long): F[Option[EmployeeSummary]] = {
    val program: ConnectionIO[EmployeeSummary] = for {
      employee <- EmployeeSQL.selectEmployee(employeeId).unique
      pos <- EmployeeSQL.selectPosition(employee.positionId).unique
      vacs <- EmployeeSQL.selectEmployeeVacsCurrY(employee.employeeId).list
    } yield EmployeeSummary(employee, pos, vacs)

    program.map(Option.apply).transact(xa).recoverWith {
      case UnexpectedEnd => none[EmployeeSummary].pure[F]
    }
  }

  def getEmployeesSummaryList(qryParams: SumrsQryParams = SumrsQryParams()): F[List[EmployeeSummary]] = {
    val program: ConnectionIO[List[EmployeeSummary]] = for {
      employees <- EmployeeSQL.selectEmployees().list
      employeesSummary <- employees.reverse.traverse { empl =>
        for {
          pos <- EmployeeSQL.selectPosition(empl.positionId).unique
          vacs <- EmployeeSQL.selectEmployeeVacsCurrY(empl.employeeId).list
        } yield EmployeeSummary(empl, pos, vacs)
      }
    } yield filterOrderEmplsSmryList(employeesSummary, qryParams) 

    program.transact(xa)
  }

  def filterOrderEmplsSmryList(emplsSumrList: List[EmployeeSummary], qryParams: SumrsQryParams): List[EmployeeSummary] = {
    val go = (dp: DateParams, vd: LocalDate) => 
      dp.before.map(b => vd.isBefore(b)).getOrElse(true) &
      dp.exact.map(e => vd.isEqual(e)).getOrElse(true) &
      dp.after.map(a => vd.isAfter(a)).getOrElse(true)
    
    val filters = Seq(
      qryParams.employeeId.map(emplId => (sumry: EmployeeSummary) => sumry.employeeId == emplId),

      qryParams.firstName.map(fn => (sumry: EmployeeSummary) => sumry.firstName == fn),
      qryParams.lastName.map(ln => (sumry: EmployeeSummary) => sumry.lastName == ln),

      qryParams.positionId.map(posId => (sumry: EmployeeSummary) => sumry.positionId == posId),
      qryParams.positionTitle.map(posTitle => (sumry: EmployeeSummary) => sumry.positionTitle == posTitle),

      qryParams.isOnVac.map(is => (sumry: EmployeeSummary) => sumry.isOnVacation == is),

      qryParams.pastVacsSince.map(dateParams => 
        (sumry: EmployeeSummary) => sumry.pastVacations.exists((vac: Vacation) => go(dateParams, vac.since))),
      qryParams.pastVacsUntil.map(dateParams => 
        (sumry: EmployeeSummary) => sumry.pastVacations.exists((vac: Vacation) => go(dateParams, vac.until))),

      qryParams.currVacSince.map(dateParams => 
        (sumry: EmployeeSummary) => 
          sumry.currentVacation.map((vac: Vacation) => go(dateParams, vac.since)).getOrElse(true)),
      qryParams.currVacUntil.map(dateParams => 
        (sumry: EmployeeSummary) => 
          sumry.currentVacation.map((vac: Vacation) => go(dateParams, vac.until)).getOrElse(true)),

      qryParams.futureVacsSince.map(dateParams => 
        (sumry: EmployeeSummary) => sumry.futureVacations.exists((vac: Vacation) => go(dateParams, vac.since))),
      qryParams.futureVacsUntil.map(dateParams => 
        (sumry: EmployeeSummary) => sumry.futureVacations.exists((vac: Vacation) => go(dateParams, vac.until)))
    )

    val actualFilters = filters.filter(_.isDefined).map(_.get)

    val defaultOrderBy = (sumry1: EmployeeSummary, sumry2: EmployeeSummary) => 
        sumry1.employeeId.compareTo(sumry2.employeeId) < 0

    val orderBy = qryParams.orderByParams match {
      case None => defaultOrderBy

      case Some(oBy) => oBy match {

        case (OrderByParams("employeeId", asc)) => 
          (sumry1: EmployeeSummary, sumry2: EmployeeSummary) => 
            asc == sumry1.employeeId.compareTo(sumry2.employeeId) < 0

        case OrderByParams("firstName", asc) => 
          (sumry1: EmployeeSummary, sumry2: EmployeeSummary) =>
            asc == sumry1.firstName.compareTo(sumry2.firstName) < 0

        case OrderByParams("lastName", asc) => 
          (sumry1: EmployeeSummary, sumry2: EmployeeSummary) =>
            asc == sumry1.lastName.compareTo(sumry2.lastName) < 0

        case OrderByParams("positionId", asc) => 
          (sumry1: EmployeeSummary, sumry2: EmployeeSummary) =>
            asc == sumry1.positionId.compareTo(sumry2.positionId) < 0

        case OrderByParams("remainedVacationDays", asc) => 
          (sumry1: EmployeeSummary, sumry2: EmployeeSummary) =>
            asc == sumry1.remainedVacationDaysCount.compareTo(sumry2.remainedVacationDaysCount) < 0

        case _ => defaultOrderBy
        }
    }

    emplsSumrList
      .filter(sumry => actualFilters.forall(filter => filter(sumry)))
      .sortWith(orderBy)
  }
}
object EmployeeRepoInterpreter {
  def apply[F[_]](xa: Transactor[F])(implicit F: MonadError[F, Throwable]): EmployeeRepoInterpreter[F] =
    new EmployeeRepoInterpreter(xa)
}
