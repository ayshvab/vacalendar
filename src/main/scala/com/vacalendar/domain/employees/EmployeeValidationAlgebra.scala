package com.vacalendar.domain.employees

import cats.data._
import com.vacalendar.domain.ValidationError
import com.vacalendar.domain.vacations.VacationIn

trait EmployeeValidationAlgebra[F[_]] {

  def validateEmplIn(emplIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], EmployeeIn]

  def checkEmplExist(emplId: Long): EitherT[F, NonEmptyList[ValidationError], Employee]

  def checkVacIsChangeable(emplId: Long,
                           vacId: Long): EitherT[F, NonEmptyList[ValidationError], Unit]

  def basicValidateVacIn(vacIn: VacationIn): Either[NonEmptyList[ValidationError], VacationIn]

  def validateVacInCreate(vacIn: VacationIn, 
                          empl: Employee): EitherT[F, NonEmptyList[ValidationError], VacationIn]

  def validateVacInUpdate(vacId: Long, 
                          vacIn: VacationIn, 
                          empl: Employee): EitherT[F, NonEmptyList[ValidationError], VacationIn]
}
