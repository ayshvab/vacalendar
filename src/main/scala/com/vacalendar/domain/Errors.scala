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

sealed trait ValidationError
case object EmployeeNotFoundError extends ValidationError
case object FirstNameHasSpecialCharactersError extends ValidationError
case object LastNameHasSpecialCharactersError extends ValidationError
case object NonExistingPositionError extends ValidationError
case object NonExistingStatusError extends ValidationError




