package com.vacalendar.domain

import java.time.{ LocalDate, Clock }
import com.vacalendar.validation.ValidationRules

case class EmployeeSummary(employeeId: Long,
                           firstName: String,
                           lastName: String,
                           positionId: Long,
                           positionTitle: String,
                           remainedVacationDaysCount: Long,
                           isOnVacation: Boolean,
                           pastVacations: List[Vacation],
                           currentVacation: Option[Vacation],
                           futureVacations: List[Vacation])

object EmployeeSummary {
  def apply(employee: Employee, pos: Position, vacs: List[Vacation], clock: Clock = Clock.systemUTC()): EmployeeSummary = {
    val vacsBeforeNow = vacs.filter { _.until.isBefore(LocalDate.now(clock)) }
    val vacsAfterNow = vacs.filter { _.since.isAfter(LocalDate.now(clock)) }
    val currVac = vacs.find { v => 
      (v.since.isBefore(LocalDate.now(clock)) || v.since.equals(LocalDate.now(clock))) &&
        (v.until.isAfter(LocalDate.now(clock)) || v.until.equals(LocalDate.now(clock)))
    }
    val vacsDaysCount = vacs.map(vac => vac.until.toEpochDay - vac.since.toEpochDay).sum
    val remainedVacDaysCount = ValidationRules.vacDaysMaxCountPerYear - vacsDaysCount
    val isOnVacation = currVac.isDefined
    
    EmployeeSummary(employee.employeeId,
                    employee.firstName,
                    employee.lastName,
                    pos.positionId,
                    pos.title,
                    remainedVacDaysCount,
                    isOnVacation,
                    vacsBeforeNow,
                    currVac,
                    vacsAfterNow)
  }
}