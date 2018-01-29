package com.vacalendar.repository

import java.time.Instant

import cats._
import com.vacalendar.domain.employees._
import doobie._
import doobie.implicits._

object EmployeeSQL {

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

  def insert(employeeNew: EmployeeNew): Query0[Employee] =
    sql"""
      INSERT INTO employees (first_name, last_name, position_id)
      VALUES (${employeeNew.firstName}, ${employeeNew.lastName}, ${employeeNew.positionId})
      RETURNING *
    """.query[Employee]

  def update(id: Long, employeeMergedWithUpd: Employee): Query0[Employee] =
    sql"""
      UPDATE employees
      SET
        first_name = ${employeeMergedWithUpd.firstName},
        last_name = ${employeeMergedWithUpd.lastName},
        position_id = ${employeeMergedWithUpd.positionId},
        status = ${employeeMergedWithUpd.status},
        updated = ${Instant.now()}
      WHERE employee_id = $id
      RETURNING *
    """.query[Employee]

  def select(id: Long): Query0[Employee] =
    sql"""
      SELECT employee_id, first_name, last_name, position_id, status, created, updated
      FROM employees
      WHERE employee_id = $id
    """.query[Employee]

  def selectAll: Query0[Employee] =
    sql"""
      SELECT employee_id, first_name, last_name, position_id, status, created, updated
      FROM employees
      ORDER BY employee_id
    """
    .query[Employee]

  def delete(id: Long) : Update0 =
    sql"""
    DELETE FROM employees WHERE employee_id = $id
    """.update

}

class EmployeeRepoInterpreter[F[_]: Monad](val xa: Transactor[F])
  extends EmployeeRepoAlgebra[F] {

  def create(employeeNew: EmployeeNew): F[Employee] = EmployeeSQL.insert(employeeNew).unique.transact(xa)

  def update(id: Long, employeeMergedWithUpd: Employee): F[Employee] = EmployeeSQL.update(id, employeeMergedWithUpd).unique.transact(xa)

  def list(): F[List[Employee]] = EmployeeSQL.selectAll.list.transact(xa)

  def get(id: Long): F[Option[Employee]] = EmployeeSQL.select(id).option.transact(xa)

  def delete(id: Long): F[Int] = EmployeeSQL.delete(id).run.transact(xa)
}

object EmployeeRepoInterpreter {
  def apply[F[_]: Monad](xa: Transactor[F]): EmployeeRepoInterpreter[F] =
    new EmployeeRepoInterpreter(xa)
}
