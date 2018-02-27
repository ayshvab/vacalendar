package com.vacalendar.domain.employees

import java.time.LocalDate

import com.vacalendar.domain.vacations.{ Vacation, VacationIn }
import com.vacalendar.endpoint.EmplsQryParams
import com.vacalendar.endpoint.VacsQryParams
import com.vacalendar.endpoint.SumrsQryParams

trait EmployeeRepoAlgebra[F[_]] {
  def createEmployee(employeeIn: EmployeeIn): F[Employee]

  def updateEmployee(employeeId: Long, employeeIn: EmployeeIn): F[Option[Employee]]

  def listEmployees(qryParams: EmplsQryParams): F[List[Employee]]

  def getEmployee(employeeId: Long): F[Option[Employee]]

  def deleteEmployee(employeeId: Long): F[Option[Employee]]

  def getVac(employeeId: Long, vacId: Long): F[Option[Vacation]]

  def getVacs(employeeId: Long, qryParams: VacsQryParams): F[List[Vacation]]

  def getEmplVacsCurrYear(employeeId: Long): F[List[Vacation]]
  
  def getOverlappedPosIdVacs(posId: Long,
                             startVac: LocalDate,
                             endVac: LocalDate): F[List[Vacation]]

  def getEmplsByPosId(posId: Long): F[List[Employee]]

  def deleteVac(emplId: Long, vacId: Long): F[Option[Vacation]]

  def createVac(emplId: Long, vacNew: VacationIn): F[Vacation]

  def updateVac(emplId: Long, vacId: Long, vacIn: VacationIn): F[Option[Vacation]]

  def getEmployeeSummary(employeeId: Long): F[Option[EmployeeSummary]]

  def getEmployeesSummaryList(qryParams: SumrsQryParams): F[List[EmployeeSummary]]

}
