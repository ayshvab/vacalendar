package com.vacalendar.domain.employees

import java.time.Instant

case class Employee(employeeId: Long,
                    firstName: String,
                    lastName: String,
                    positionId: Long,
                    status: EmployeeStatus = Active,
                    created: Instant,
                    updated: Option[Instant] = None)

sealed trait EmployeeStatus extends Product with Serializable
case object Active extends EmployeeStatus
case object OnVacation extends EmployeeStatus
case object Fired extends EmployeeStatus

object EmployeeStatus {
  def apply(name: String): EmployeeStatus = name match {
    case "active" => Active
    case "onVacation" => OnVacation
    case "fired" => Fired
  }

  def nameOf(status: EmployeeStatus): String = status match {
    case Active => "active"
    case OnVacation => "onVacation"
    case Fired => "fired"
  }
}
