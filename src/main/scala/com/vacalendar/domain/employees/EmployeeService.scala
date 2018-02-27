package com.vacalendar.domain.employees

import cats.data._
import cats.implicits._
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

  def createEmployee(emplIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], Employee] =
    for {
      validEmplIn <- validation.validateEmplIn(emplIn)
      createdEmployee <- EitherT.liftF(employeeRepo.createEmployee(validEmplIn))
    } yield createdEmployee

  def updateEmployee(emplId: Long, emplIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], Employee] =
    for {
      _ <- validation.checkEmplExist(emplId)
      validEmplIn <- validation.validateEmplIn(emplIn)
      updatedEmployee <- EitherT.fromOptionF(
        employeeRepo.updateEmployee(emplId, validEmplIn),
        NonEmptyList.one[ValidationError](EmployeeNotFoundError))
    } yield updatedEmployee

  // ==> Vacations <==

  def getVac(employeeId: Long, vacId: Long): EitherT[F, VacationNotFoundError.type, Vacation] =
    EitherT.fromOptionF(employeeRepo.getVac(employeeId, vacId), VacationNotFoundError)

  def getVacs(employeeId: Long, qryParams: VacsQryParams): EitherT[F, NonEmptyList[ValidationError], List[Vacation]] =
    for {
      _ <- validation.checkEmplExist(employeeId)
      vacations <- EitherT.liftF(employeeRepo.getVacs(employeeId, qryParams))
    } yield vacations

  def deleteVac(employeeId: Long, vacId: Long): EitherT[F, NonEmptyList[ValidationError], Vacation] =
    for {
      _ <- validation.checkVacIsChangeable(employeeId, vacId)
      vac <- EitherT.fromOptionF[F, NonEmptyList[ValidationError], Vacation](
        employeeRepo.deleteVac(employeeId, vacId), NonEmptyList.one(VacationNotFoundError))
    } yield vac

  def createVac(emplId: Long, vacIn: VacationIn): EitherT[F, NonEmptyList[ValidationError], Vacation] = {
    for {
      _   <- EitherT.liftF(L.info(s"Input vacationIn: $vacIn"))

      empl <- validation.checkEmplExist(emplId)
      
      res <- EitherT[F, NonEmptyList[ValidationError], VacationIn](
        validation.basicValidateVacIn(vacIn).pure[F])
      _   <- EitherT.liftF(L.info(s"VacIn pass basic validation"))

      validVacIn <- validation.validateVacInCreate(res, empl)
      _   <- EitherT.liftF(L.info(s"VacIn: $validVacIn pass validation for create new vacation"))

      created <- EitherT.liftF(employeeRepo.createVac(emplId, validVacIn))
      _   <- EitherT.liftF(L.info(s"New vacation created: $created"))
    } yield created
  }

  def updateVac(emplId: Long, vacId: Long, vacIn: VacationIn): EitherT[F, NonEmptyList[ValidationError], Vacation] = {
    for {
      _   <- EitherT.liftF(L.info(s"Input vacationIn: $vacIn"))

      _ <- validation.checkVacIsChangeable(emplId, vacId)

      empl <- validation.checkEmplExist(emplId)

      res <- EitherT[F, NonEmptyList[ValidationError], VacationIn](
        validation.basicValidateVacIn(vacIn).pure[F])
      _   <- EitherT.liftF(L.info(s"VacIn pass basic validation"))

      validVacIn <- validation.validateVacInUpdate(vacId, res, empl)
      _   <- EitherT.liftF(L.info(s"VacIn: $validVacIn pass validation for create new vacation"))

      updated <- EitherT.fromOptionF[F, NonEmptyList[ValidationError], Vacation](
        employeeRepo.updateVac(emplId, vacId, vacIn), NonEmptyList.one(VacationNotFoundError))

    } yield updated
  }
  
}

object EmployeeService {
  def apply[F[_]: Effect](employeeRepo: EmployeeRepoAlgebra[F],
                  validation: EmployeeValidationAlgebra[F]) =

    new EmployeeService[F](employeeRepo, validation)
}
