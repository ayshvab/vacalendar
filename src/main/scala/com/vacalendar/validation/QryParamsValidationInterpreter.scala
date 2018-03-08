package com.vacalendar.validation

import java.time.LocalDate

import cats.data._
import cats.implicits._

import com.vacalendar.errors._
import com.vacalendar.endpoints.QryParams._

object QryParamsValidationInterpreter extends QryParamsValidationAlgebra {

  type ValidationResult[A] = ValidatedNel[QryParamsValidationError, A]

  val emplsOrderFieldsToDB = Map("employeeId" -> "employee_id",
                                 "positionId" -> "position_id",
                                 "firstname" -> "first_name",
                                 "lastname" -> "last_name",
                                 "created" -> "created",
                                 "updated" -> "updated")

  val vacsOrderFieldsToDB = Map("vacationId" -> "vacation_id",
                                "employeeId" -> "employee_id",
                                "since" -> "since",
                                "until" -> "until",
                                "created" -> "created",
                                "updated" -> "updated")

  val sumrsOrderFields = List("employeeId", "firstName", "lastName", "positionId", "remainedVacationDays")
  val sumrsOrderFieldsMap = sumrsOrderFields.zip(sumrsOrderFields).toMap

  private def validateAndPrepareOrderBy(rawOrderBy: Option[String], fieldsForOrder: Map[String, String]): ValidationResult[Option[OrderByParams]] = {
    val preparedField = rawOrderBy flatMap { s =>
      val normalized = s.stripPrefix("-")
      fieldsForOrder.get(normalized)
    }

    rawOrderBy match {
      case None => None.validNel
      case Some(s) if s.startsWith("-") => {
        if (preparedField.isDefined) Some(OrderByParams(preparedField.get, asc = false)).validNel
        else NotValidQueryParam(s).invalidNel
      }
      case Some(_) if (preparedField.isDefined) => Some(OrderByParams(preparedField.get, asc = true)).validNel
      case Some(s) => NotValidQueryParam(s).invalidNel
    }
  }

  def validateAndPrepareEmplsQryParams(oBy: Option[String],
                                      fn: Option[String],
                                      ln: Option[String],
                                      posId: Option[Long]): Either[NonEmptyList[QryParamsValidationError], EmplsQryParams] = {

    val result = (validateAndPrepareOrderBy(oBy, emplsOrderFieldsToDB),
                  fn.validNel[QryParamsValidationError],
                  ln.validNel[QryParamsValidationError],
                  posId.validNel[QryParamsValidationError]) mapN (EmplsQryParams.apply)

    result.toEither
  }

  private def validateAndPrepareLocalDateStr(raw: Option[String]): ValidationResult[Option[DateParams]] = {
    val prefix = "_.."
    val suffix = ".._"
    val sep = Array('.', '.')
    raw match {
      case None => None.validNel
      case Some("_.._") => None.validNel
      case Some(s) if (s.startsWith(prefix)) => Either.catchNonFatal(LocalDate.parse(s.stripPrefix(prefix))) match {
        case Left(_) => NotValidLocalDateString.invalidNel
        case Right(b@_) => Some(DateParams(before = Some(b))).validNel
      }
      case Some(s) if (s.endsWith(suffix)) => Either.catchNonFatal(LocalDate.parse(s.stripSuffix(suffix))) match {
        case Left(_) => NotValidLocalDateString.invalidNel
        case Right(a@_) => Some(DateParams(after = Some(a))).validNel
      }
      case Some(s) if (s.contains(sep)) => {
        Either.catchNonFatal {
            val Array(a, b) = s.split(sep).filter(_.nonEmpty)
            DateParams(after = Some(LocalDate.parse(a)), before = Some(LocalDate.parse(b)))
        } match {
            case Left(_) => NotValidLocalDateString.invalidNel
            case Right(dateParams) => Some(dateParams).validNel
        }
      }
      case Some(s) => Either.catchNonFatal(LocalDate.parse(s)) match {
        case Left(_) =>NotValidLocalDateString.invalidNel
        case Right(date) => Some(DateParams(exact = Some(date))).validNel
      }
      case _ => NotValidLocalDateString.invalidNel
    }
  }

  private def validateAndPrepareIsOnVac(raw: Option[String]): ValidationResult[Option[Boolean]] = {
    raw match {
      case None => None.validNel
      case Some("t") => Some(true).validNel
      case Some("f") => Some(false).validNel
      case Some(s) =>  NotValidQueryParam(s).invalidNel
    }
  }

  def validateAndPrepareVacsQryParams(rawOrderBy: Option[String],
                                      since: Option[String],
                                      until: Option[String]): Either[NonEmptyList[QryParamsValidationError], VacsQryParams] = {

    val result = (validateAndPrepareOrderBy(rawOrderBy, vacsOrderFieldsToDB),
                  validateAndPrepareLocalDateStr(since),
                  validateAndPrepareLocalDateStr(until)) mapN(VacsQryParams)

    result.toEither
  }

  def validateAndPrepareEmplSummariesQryParams(oBy: Option[String],
                                       emplId: Option[Long],
                                       fn: Option[String],
                                       ln: Option[String],
                                       posId: Option[Long],
                                       posTitle: Option[String],
                                       isOnVac: Option[String],
                                       
                                       pastVacsSince: Option[String],
                                       pastVacsUntil: Option[String],
                                       
                                       currVacSince: Option[String],
                                       currVacUntil: Option[String],
                                       
                                       futureVacsSince: Option[String],
                                       futureVacsUntil: Option[String]): Either[NonEmptyList[QryParamsValidationError], EmplSummariesQryParams] = {

    val result = (validateAndPrepareOrderBy(oBy, sumrsOrderFieldsMap),
                  emplId.validNel[QryParamsValidationError],
                  fn.validNel[QryParamsValidationError],
                  ln.validNel[QryParamsValidationError],
                  posId.validNel[QryParamsValidationError],
                  posTitle.validNel[QryParamsValidationError],

                  validateAndPrepareIsOnVac(isOnVac),

                  validateAndPrepareLocalDateStr(pastVacsSince),
                  validateAndPrepareLocalDateStr(pastVacsUntil),

                  validateAndPrepareLocalDateStr(currVacSince),
                  validateAndPrepareLocalDateStr(currVacUntil),

                  validateAndPrepareLocalDateStr(futureVacsSince),
                  validateAndPrepareLocalDateStr(futureVacsUntil)) mapN(EmplSummariesQryParams.apply)

    result.toEither
  }
}

