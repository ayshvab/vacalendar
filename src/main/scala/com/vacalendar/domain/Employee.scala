package com.vacalendar.domain

import java.time.Instant

case class Employee(employeeId: Long,
                    firstName: String,
                    lastName: String,
                    positionId: Long,
                    created: Instant,
                    updated: Option[Instant] = None)

case class EmployeeIn(firstName: String,
                      lastName: String,
                      positionId: Long)