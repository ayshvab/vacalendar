package com.vacalendar.domain.employees

trait EmployeeRepoAlgebra[F[_]] {
  def list(): F[List[Employee]]

  def get(id: Long): F[Option[Employee]]

  def create(employeeNew: EmployeeNew): F[Employee]

  def update(id: Long, employeeMergedWithUpd: Employee): F[Employee]

  def delete(id: Long): F[Int]
}
