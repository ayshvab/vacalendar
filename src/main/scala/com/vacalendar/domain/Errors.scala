package com.vacalendar.domain

//  // ==> Errors <==
//  class ErrorCode(val value: String) extends AnyVal // or Enumeration of ErrorCodes
//
//  trait Error {
//    val code: ErrorCode
//    val message: String
////    val target
////    val details
////    val innererror
//  }
//
//  sealed abstract class ServiceError extends Error
//  sealed abstract class DatabaseError extends Error
//  sealed abstract class RequestError extends Error
//
//  sealed abstract class ApiError
//  object ApiError {
////    final case class Database(err: DatabaseErr) extends ApiError
//    final case class Service(err: ServiceError) extends ApiError
////    final case class Request(err: RequestErr) extends ApiError
//  }

//sealed trait ValidationError
sealed abstract class ValidationError
case object EmployeeNotFoundError extends ValidationError
case object FirstNameHasSpecialCharactersError extends ValidationError
case object LastNameHasSpecialCharactersError extends ValidationError
case object NonExistingPositionError extends ValidationError

case object VacationNotFoundError extends ValidationError

case object MustNotDeleteOrUpdateVacationAfterStartError extends ValidationError


case object NotFoundError extends ValidationError

case object VacationMustStartBeforeUntilError extends ValidationError

case object VacationMustStartAndEndOnOneYear extends ValidationError
case object VacationPeriodOutOfMinBoundError extends ValidationError
case object VacationPeriodOutOfMaxBoundError extends ValidationError
case object VacataionMustBeOnlyInFutureError extends ValidationError
case object VacationMustNotOverlappedError extends ValidationError
case object OutOfTotalVacationDaysPerYearError extends ValidationError
case object NotEnoughDaysPassFromLastVacationError extends ValidationError
case object NotEnoughDaysToNextVacationError extends ValidationError
case object TooManyEmployeesOfOnePositionOnVacationError extends ValidationError
case object CannotChangeOrDeleteCurrentOrFutureVacationsError extends ValidationError
case object NotIdenticalEmployeeIdsError extends ValidationError

case object NotValidOrderByParamError extends ValidationError
case object LocalDateParseError extends ValidationError