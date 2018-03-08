package com.vacalendar.validation

import cats.data._

import com.vacalendar.errors.ServiceValidationError
import com.vacalendar.domain._

trait ServiceValidationAlgebra {

  def validateEmplIn(emplIn: EmployeeIn, 
                     foundPos: Option[Position]): Either[NonEmptyList[ServiceValidationError], EmployeeIn]

  def checkEmplExist(foundEmpl: Option[Employee]): Either[ServiceValidationError, Employee]


  def checkVacIsChangeable(foundVac: Option[Vacation]): Either[ServiceValidationError, Vacation]


  def basicValidateVacIn(vacIn: VacationIn): Either[NonEmptyList[ServiceValidationError], VacationIn]

  def validateVacInCreate(vacIn: VacationIn, 
                          emplVacsCurrYear: List[Vacation],
                          overlappedVacsWithSamePosId: List[Vacation],
                          emplsWithSamePosId: List[Employee]): Either[NonEmptyList[ServiceValidationError], VacationIn]

  def validateVacInUpdate(vacIn: VacationIn,
                          vacId: Long,
                          emplVacsCurrYear: List[Vacation],
                          overlappedVacsWithSamePosId: List[Vacation],
                          emplsWithSamePosId: List[Employee]): Either[NonEmptyList[ServiceValidationError], VacationIn]

}
