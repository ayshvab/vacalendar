package com.vacalendar.domain.employees

trait EmployeeRepoAlgebra[F[_]] {
  def list(): F[List[Employee]]
}
