package com.vacalendar.domain.employees

import cats.data._
import cats.effect.Effect
import com.vacalendar.domain.{EmployeeNotFoundError, ValidationError}
import com.vacalendar.domain.VacationNotFoundError
import com.vacalendar.domain.vacations.{Vacation, VacationIn}
import com.vacalendar.common.Log
import com.vacalendar.endpoint.EmplsQryParams
import com.vacalendar.endpoint.VacsQryParams
import com.vacalendar.endpoint.SumrsQryParams


class EmployeeService[F[_]: Effect](employeeRepo: EmployeeRepoAlgebra[F],
                                    validation: EmployeeValidationAlgebra[F])
                                   (implicit L: Log[F]) {

  def getEmployeeSummary(employeeId: Long): EitherT[F, EmployeeNotFoundError.type, EmployeeSummary] = 
    EitherT.fromOptionF(employeeRepo.getEmployeeSummary(employeeId), EmployeeNotFoundError)

  def getEmployeesSummaryList(qryParams: SumrsQryParams): F[List[EmployeeSummary]] = employeeRepo.getEmployeesSummaryList(qryParams)

  def listEmployees(qryParams: EmplsQryParams): F[List[Employee]] = employeeRepo.listEmployees(qryParams)

  def getEmployee(employeeId: Long): EitherT[F, EmployeeNotFoundError.type, Employee] =
    EitherT.fromOptionF(employeeRepo.getEmployee(employeeId), EmployeeNotFoundError)

  def deleteEmployee(employeeId: Long): EitherT[F, EmployeeNotFoundError.type, Employee] =
    EitherT.fromOptionF[F, EmployeeNotFoundError.type, Employee](employeeRepo.deleteEmployee(employeeId), EmployeeNotFoundError)

  def createEmployee(employeeIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], Employee] =
    for {
      checkedEmployeeIn <- validation.checkCreateEmployee(employeeIn)
      createdEmployee <- EitherT.liftF(employeeRepo.createEmployee(checkedEmployeeIn))
    } yield createdEmployee

  def updateEmployee(employeeId: Long, employeeIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], Employee] =
    for {
      checkedEmployeeIn <- validation.checkUpdateEmployee(employeeId, employeeIn)
      updatedEmployee <- EitherT.fromOptionF(
        employeeRepo.updateEmployee(employeeId, checkedEmployeeIn),
        NonEmptyList.one[ValidationError](EmployeeNotFoundError))
    } yield updatedEmployee

  // ==> Vacations <==

  def getVac(employeeId: Long, vacId: Long): EitherT[F, VacationNotFoundError.type, Vacation] =
    EitherT.fromOptionF(employeeRepo.getVac(employeeId, vacId), VacationNotFoundError)

  def getVacs(employeeId: Long, qryParams: VacsQryParams): EitherT[F, NonEmptyList[ValidationError], List[Vacation]] =
    for {
      _ <- validation.checkEmployeeExist(employeeId)
      vacations <- EitherT.liftF(employeeRepo.getVacs(employeeId, qryParams))
    } yield vacations

  def deleteVac(employeeId: Long, vacId: Long): EitherT[F, NonEmptyList[ValidationError], Vacation] =
    for {
      _ <- validation.checkIsChangeableVac(employeeId, vacId)
      vac <- EitherT.fromOptionF[F, NonEmptyList[ValidationError], Vacation](
        employeeRepo.deleteVac(employeeId, vacId), NonEmptyList.one(VacationNotFoundError))
    } yield vac

  def createVac(employeeId: Long, vacIn: VacationIn): EitherT[F, NonEmptyList[ValidationError], Vacation] = {

    for {
      _   <- EitherT.liftF(L.info(s"Input vacationIn: $vacIn"))
      validVacIn <- validation.checkCreateVac(employeeId, vacIn)
      _   <- EitherT.liftF(L.info(s"Validated vacationIn: $validVacIn"))
      created <- EitherT.liftF(employeeRepo.createVac(validVacIn))
    } yield created
  }

  def updateVac(employeeId: Long, vacId: Long, vacIn: VacationIn): EitherT[F, NonEmptyList[ValidationError], Vacation] = {

    for {
      validVacIn <- validation.checkUpdateVac(employeeId, vacId, vacIn)
      updated <- EitherT.fromOptionF[F, NonEmptyList[ValidationError], Vacation](
        employeeRepo.updateVac(vacId, vacIn), NonEmptyList.one(VacationNotFoundError))
    } yield updated
  }
  
}

object EmployeeService {
  def apply[F[_]: Effect](employeeRepo: EmployeeRepoAlgebra[F],
                  validation: EmployeeValidationAlgebra[F]) =

    new EmployeeService[F](employeeRepo, validation)
}
