package com.vacalendar.errors

import cats.data._

trait Err {
  val code: String
  val message: String
}

sealed abstract class QryParamsValidationError extends Err
sealed abstract class ServiceValidationError extends Err

sealed abstract class AppError
object AppError {
  
  final case class QryParamsValidationErrsWrapper(errors: NonEmptyList[QryParamsValidationError]) extends AppError

  final case class ServiceValidationErrWrapper(error: ServiceValidationError) extends AppError
  final case class ServiceValidationErrsWrapper(errors: NonEmptyList[ServiceValidationError]) extends AppError

  final case class DbErrWrapper(err: Throwable) extends AppError
}

case object EmplNotFound extends ServiceValidationError {
  val code = "EmplNotFound"
  val message = s"Employee not found"
}

case class FirstNameHasSpecialCharacters(fn: String) extends ServiceValidationError {
  val code = "FirstNameHasSpecialCharacters"
  val message = s"First name: $fn has special characters"
}

case class LastNameHasSpecialCharacters(ln: String) extends ServiceValidationError {
  val code = "LastNameHasSpecialCharacters"
  val message = s"Last name: $ln has special characters"
}

case object PosNotFound extends ServiceValidationError {
  val code = "PosNotFound"
  val message = s"Position not found"
}

case object VacNotFound extends ServiceValidationError {
  val code = "VacNotFound"
  val message = s"Vacation not found"
}

case object VacSinceDateMustBeforeUntilDate extends ServiceValidationError {
  val code = "VacSinceDateMustBeforeUntilDate"
  val message = "Since date must be before until date"
}

case object VacMustStartAndEndWithin1Year extends ServiceValidationError {
  val code = "VacMustStartAndEndWithin1Year"
  val message = "Vacation must start and end within one year"
}

case object VacPeriodIsLessMin extends ServiceValidationError {
  val code = "VacPeriodIsLessMin"
  val message = "Vacation period is less than the minimum"
}

case object VacPeriodIsMoreMax extends ServiceValidationError {
  val code = "VacPeriodIsMoreMax"
  val message = "Vacation period is more than the maximum"
}

case object VacOnlyInFuture extends ServiceValidationError {
  val code = "VacOnlyInFuture"
  val message = "New or updated vacation must be only in future"
}

case object VacsMustNotOverlap extends ServiceValidationError {
  val code = "VacsMustNotOverlap"
  val message = "Employee vacations must not overlap with each other"
}

case object NotEnoughDaysPassFromLastVac extends ServiceValidationError {
  val code = "NotEnoughDaysPassFromLastVac"
  val message = "The minimum period between vacation periods is equal to the size of the first vacation"
}

case object NotEnoughDaysToNextVac extends ServiceValidationError {
  val code = "NotEnoughDaysToNextVac"
  val message = "The minimum period between vacation periods is equal to the size of the first vacation"
}

case object MaxCountVacDaysPerYearExceeded extends ServiceValidationError {
  val code = "MaxCountVacDaysPerYearExceeded"
  val message = "The maximum count of vacation days per year is exceeded"
}

case object TooManyEmplsOfOnePosOnVac extends ServiceValidationError {
  val code = "TooManyEmplsOfOnePosOnVac"
  val message = "Too many employees of one position will be on vacation at the same time"
}

case object CannotChangeOrDeleteNotFutureVac extends ServiceValidationError {
  val code = "CannotChangeOrDeleteNotFutureVac"
  val message = "Only the upcoming vacation can be deleted or modified"
}

case class NotValidQueryParam(value: String) extends QryParamsValidationError {
  val code = "NotValidQueryParam"
  val message = s"Not valid query param value: $value"
}

case object NotValidLocalDateString extends QryParamsValidationError {
  val code = "NotValidLocalDateString"
  val message = s"Not valid date string"
}

