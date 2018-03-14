package com.vacalendar.endpoints

import java.time.LocalDate

// import org.http4s._
// import org.http4s.dsl.Http4sDsl

// object QryParamsMatchers extends Http4sDsl[F] { // TODO: remove this
//   object OrderByQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("orderBy")

//   object FirstNameQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("firstname")
//   object LastNameQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("lastname")
//   object PositionIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("positionId")
//   object PositionTitleQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("positionTitle")
//   object VacationIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("vacationId")
//   object EmployeeIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("employeeId")

//   // object SinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("since")
//   // object UntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("until")

//   object OnVacQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("isOnVacacation")
  
//   object CurrVacSinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("currVac.since")
//   object CurrVacUntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("currVac.until")
  
//   object PastVacsSinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("pastVacs.since")
//   object PastVacsUntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("pastVacs.until")

//   object FutureVacsSinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("futureVacs.since")
//   object FutureVacsUntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("futureVacs.until")
// }

object QryParams {

  case class EmplsQryParams(orderByParams: Option[OrderByParams] = None,
                            firstName: Option[String] = None,
                            lastName: Option[String] = None,
                            positionId: Option[Long] = None)

  case class VacsQryParams(orderByParams: Option[OrderByParams] = None,
                           since: Option[DateParams] = None,
                           until: Option[DateParams] = None )

  case class EmplSummariesQryParams(orderByParams: Option[OrderByParams] = None,
                                    employeeId: Option[Long] = None,
                                    firstName: Option[String] = None,
                                    lastName: Option[String] = None,
                                    positionId: Option[Long] = None,
                                    positionTitle: Option[String] = None,
                                    isOnVac: Option[Boolean] = None,

                                    pastVacsSince: Option[DateParams] = None,
                                    pastVacsUntil: Option[DateParams] = None,

                                    currVacSince: Option[DateParams] = None,
                                    currVacUntil: Option[DateParams] = None,

                                    futureVacsSince: Option[DateParams] = None,
                                    futureVacsUntil: Option[DateParams] = None)

  case class OrderByParams(field: String, asc: Boolean)

  case class DateParams(before: Option[LocalDate] = None, 
                        exact: Option[LocalDate] = None, 
                        after: Option[LocalDate] = None)
}