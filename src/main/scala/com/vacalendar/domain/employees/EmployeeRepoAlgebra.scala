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

  def getEmployeeVacsCurrY(employeeId: Long): F[List[Vacation]]

  def getEmployeeVacsOverlappedPosIdVacsForCurrY(posId: Long,
                                                 startVac: LocalDate,
                                                 endVac: LocalDate): F[List[Vacation]]

  def getEmployeesByPosId(posId: Long): F[List[Employee]]

  def deleteVac(employeeId: Long, vacId: Long): F[Option[Vacation]]

  def createVac(vacNew: VacationIn): F[Vacation]

  def updateVac(vacId: Long, vacIn: VacationIn): F[Option[Vacation]]

  def getEmployeeSummary(employeeId: Long): F[Option[EmployeeSummary]]

  def getEmployeesSummaryList(qryParams: SumrsQryParams): F[List[EmployeeSummary]]
}
