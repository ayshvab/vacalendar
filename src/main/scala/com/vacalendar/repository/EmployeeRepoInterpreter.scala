package com.vacalendar.repository

import java.time.Instant

import cats.Monad
import com.vacalendar.domain.employees.{Employee, EmployeeRepoAlgebra, EmployeeStatus}
import doobie.util.meta.Meta
import doobie.util.transactor.Transactor

import cats._
//import cats.data._
//import cats.implicits._
import doobie._

import doobie.implicits._

class EmployeeRepoInterpreter[F[_]: Monad](val xa: Transactor[F])
  extends EmployeeRepoAlgebra[F] {

  implicit val StatusMeta: Meta[EmployeeStatus] =
    Meta[String].xmap(EmployeeStatus.apply, EmployeeStatus.nameOf)

  implicit val InstantMeta: Meta[Instant] =
    Meta[java.sql.Timestamp].xmap(
      ts => ts.toInstant,
      inst => java.sql.Timestamp.from(inst)
    )

//  implicit val LocalDateMeta: Meta[LocalDate] =
//    Meta[java.sql.Date].xmap(
//      sqlDate => sqlDate.toLocalDate,
//      ld => java.sql.Date.valueOf(ld)
//    )

  def list(): F[List[Employee]] =
    sql"""SELECT employee_id, first_name, last_name, position_id, status, created, updated
         |FROM employees
         |ORDER BY employee_id"""
    .query[Employee]
    .list
    .transact(xa)
}

object EmployeeRepoInterpreter {
  def apply[F[_]: Monad](xa: Transactor[F]): EmployeeRepoInterpreter[F] =
    new EmployeeRepoInterpreter(xa)
}
