package com.vacalendar.domain.employees

import cats.data._
import com.vacalendar.domain.ValidationError

trait EmployeeValidationAlgebra[F[_]] {
  def checkEmployeeNew(employeeNew: EmployeeNew): EitherT[F, NonEmptyList[ValidationError], EmployeeNew]

  def checkEmployeeUpd(id: Long, employeeUpd: EmployeeUpd): EitherT[F, NonEmptyList[ValidationError], Employee]
}
