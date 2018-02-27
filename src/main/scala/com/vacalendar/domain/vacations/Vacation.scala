package com.vacalendar.domain.vacations

import java.time.{Instant, LocalDate}

case class Vacation(vacationId: Long,
                    employeeId: Long,
                    since: LocalDate,
                    until: LocalDate,
                    created: Instant,
                    updated: Option[Instant] = None)

case class VacationIn(since: LocalDate,
                      until: LocalDate)
