package com.vacalendar.domain.employees

import java.time.Instant

case class Employee(employeeId: Long,
                    firstName: String,
                    lastName: String,
                    positionId: Long,
                    status: EmployeeStatus = Active,
                    created: Instant,
                    updated: Option[Instant] = None)

object Employee {
  def update(oldEmployee: Employee, firstName: String, lastName: String, positionId: Long, status: String): Employee = {
    oldEmployee.copy(
      firstName = firstName,
      lastName = lastName,
      positionId = positionId,
      status = EmployeeStatus(status),
      updated = Some(Instant.now())
    )
  }
}

sealed trait EmployeeStatus extends Product with Serializable
case object Active extends EmployeeStatus
case object Fired extends EmployeeStatus

object EmployeeStatus {
  def apply(name: String): EmployeeStatus = name match {
    case "Active" => Active
    case "Fired" => Fired
  }

  def nameOf(status: EmployeeStatus): String = status match {
    case Active => "Active"
    case Fired => "Fired"
  }
}

case class EmployeeNew(firstName: String,
                       lastName: String,
                       positionId: Long)


case class EmployeeUpd(firstName: String,
                       lastName: String,
                       positionId: Long,
                       status: String)


