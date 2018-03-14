package com.vacalendar.repository

import java.time.{ Instant, Clock }

import cats._
import cats.implicits._
import cats.data._
import doobie._
import doobie.implicits._
import Fragments._

import com.vacalendar.endpoints.QryParams._
import com.vacalendar.domain._
import com.vacalendar.errors._

object EmployeeSQL {

  def insertEmpl(emplIn: EmployeeIn, clock: Clock): Query0[Employee] =
    sql"""
      INSERT INTO employees (first_name, last_name, position_id, created)
      VALUES (${emplIn.firstName}, ${emplIn.lastName}, ${emplIn.positionId}, ${Instant.now(clock)})
      RETURNING *
    """.query[Employee]

  def updateEmpl(emplId: Long, emplIn: EmployeeIn, clock: Clock): Query0[Employee] =
    sql"""
      UPDATE employees
      SET
        first_name = ${emplIn.firstName},
        last_name = ${emplIn.lastName},
        position_id = ${emplIn.positionId},
        updated = ${Instant.now(clock)}
      WHERE employee_id = $emplId
      RETURNING *
    """.query[Employee]

  def selectEmpl(emplId: Long): Query0[Employee] =
    sql"""
      SELECT *
      FROM employees
      WHERE employees.employee_id = $emplId
    """.query[Employee]

  def selectEmpls(params: EmplsQryParams = EmplsQryParams()): Query0[Employee] = {
    val whereFirstName = params.firstName.map(s => fr"first_name like $s")
    val whereLastName = params.lastName.map(s => fr"last_name like $s")
    val wherePositionId = params.positionId.map(n => fr"position_id = $n")
  
    val defaultOrderBy: Fragment = fr"order by employee_id asc"

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

  def deleteEmpl(emplId: Long) : Query0[Employee] =
    sql"""
    DELETE FROM employees
    WHERE employee_id = $emplId
    RETURNING *
    """
    .query[Employee]

  def selectEmplsByPosId(posId: Long): Query0[Employee] = sql"""
      SELECT *
      FROM
        employees
      WHERE employees.position_id = $posId
      ORDER BY employees.employee_id
    """
    .query[Employee]
}

class EmployeeRepoInterpreter[F[_]](val xa: Transactor[F])
                                   (implicit F: MonadError[F, Throwable]) extends EmployeeRepoAlgebra[F] {

  def createEmpl(emplIn: EmployeeIn, clock: Clock = Clock.systemUTC()): EitherT[F, AppError, Employee] =
    EmployeeSQL.insertEmpl(emplIn, clock).unique.transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)

  def updateEmpl(emplId: Long, emplIn: EmployeeIn, clock: Clock = Clock.systemUTC()): EitherT[F, AppError, Option[Employee]] =
    EmployeeSQL.updateEmpl(emplId, emplIn, clock).option
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)

  def getEmpls(params: EmplsQryParams): EitherT[F, AppError, List[Employee]] = 
    EmployeeSQL.selectEmpls(params).to[List]
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)

  def getEmpl(emplId: Long): EitherT[F, AppError, Option[Employee]] =
    EmployeeSQL.selectEmpl(emplId).option
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)

  def getEmplsByPosId(posId: Long): EitherT[F, AppError, List[Employee]] = {
    EmployeeSQL.selectEmplsByPosId(posId).to[List]
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)
  }

  def deleteEmpl(emplId: Long): EitherT[F, AppError, Option[Employee]] =
    EmployeeSQL.deleteEmpl(emplId).option
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)

}