package com.vacalendar.domain.employees

import cats.data._
import com.vacalendar.domain.ValidationError
import com.vacalendar.domain.vacations.VacationIn

trait EmployeeValidationAlgebra[F[_]] {

  def checkEmployeeExist(employeeId: Long): EitherT[F, NonEmptyList[ValidationError], Employee]

  def checkCreateEmployee(employeeIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], EmployeeIn]

  def checkUpdateEmployee(employeeId: Long,
                          employeeIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], EmployeeIn]

  def checkCreateVac(employeeId: Long, vacIn: VacationIn): EitherT[F, NonEmptyList[ValidationError], VacationIn]

  def checkIsChangeableVac(employeeId: Long,
                           vacId: Long): EitherT[F, NonEmptyList[ValidationError], Unit]

  def checkUpdateVac(employeeId: Long,
                     vacId: Long,
                     vacIn: VacationIn): EitherT[F, NonEmptyList[ValidationError], VacationIn]
}
