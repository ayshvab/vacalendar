package com.vacalendar.endpoints

import java.time.LocalDate

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