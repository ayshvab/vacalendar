package com.vacalendar.validation

import cats.data._

import com.vacalendar.errors._
import com.vacalendar.endpoints.QryParams._

trait QryParamsValidationAlgebra {

  def validateAndPrepareEmplsQryParams(oBy: Option[String],
                                      fn: Option[String],
                                      ln: Option[String],
                                      posId: Option[Long]): Either[NonEmptyList[QryParamsValidationError], EmplsQryParams]


  def validateAndPrepareVacsQryParams(rawOrderBy: Option[String],
                                      since: Option[String],
                                      until: Option[String]): Either[NonEmptyList[QryParamsValidationError], VacsQryParams]

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
                                       futureVacsUntil: Option[String]): Either[NonEmptyList[QryParamsValidationError], EmplSummariesQryParams]

}

