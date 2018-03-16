package com.vacalendar.validation

import java.time.{ LocalDate, Clock }

import cats.data._
import cats.implicits._

import com.vacalendar.errors._
import com.vacalendar.domain._

class ServiceValidationInterpreter(clock: Clock = Clock.systemUTC()) extends ServiceValidationAlgebra {
  
  type ValidationResult[A] = ValidatedNel[ServiceValidationError, A]

  private def isVacsOverlapped(startVac1: LocalDate,
                               endVac1: LocalDate,
                               startVac2: LocalDate,
                               endVac2: LocalDate): Boolean = {

    (startVac1.isBefore(endVac2) || startVac1.isEqual(endVac2)) &&
      (startVac2.isBefore(endVac1) || startVac2.isEqual(endVac1))
  }

  private def validateFirstName(firstName: String): ValidationResult[String] =
    if (firstName.matches("^[a-zA-Z]+$")) firstName.validNel
    else FirstNameHasSpecialCharacters(firstName).invalidNel

  private def validateLastName(lastName: String): ValidationResult[String] =
    if (lastName.matches("^[a-zA-Z]+$")) lastName.validNel
    else LastNameHasSpecialCharacters(lastName).invalidNel

  private def validatePosIdExist(foundPos: Option[Position]): ValidationResult[Long] =
    if (foundPos.isDefined) foundPos.get.positionId.validNel
    else PosNotFound.invalidNel

  private def validateVacDirection(vacIn: VacationIn): ValidationResult[Unit] =
    if (vacIn.since.isBefore(vacIn.until)) ().validNel
    else VacSinceDateMustBeBeforeUntilDate.invalidNel

  private def validateVacPeriodWithin1Year(vacIn: VacationIn): ValidationResult[Unit] =
    if (vacIn.since.getYear == vacIn.until.getYear) ().validNel
    else VacMustStartAndEndWithin1Year.invalidNel

  private def validateMinVacPeriod(vacIn: VacationIn): ValidationResult[Unit] = {
    val vacPeriod = Math.abs(vacIn.until.toEpochDay - vacIn.since.toEpochDay)
    if (vacPeriod < ValidationRules.minVacPeriod) VacPeriodIsLessMin.invalidNel
    else ().validNel
  }

  private def validateMaxVacPeriod(vacIn: VacationIn): ValidationResult[Unit] = {
    val vacPeriod = Math.abs(vacIn.until.toEpochDay - vacIn.since.toEpochDay)
    if (vacPeriod > ValidationRules.maxVacPeriod) VacPeriodIsMoreMax.invalidNel
    else ().validNel
  }

  private def validateVacOnlyInFuture(vacIn: VacationIn): ValidationResult[Unit] = {
    if (vacIn.since.isAfter(LocalDate.now(clock))) ().validNel
    else VacOnlyInFuture.invalidNel
  }

  private def validateVacNotOverlap(vacIn: VacationIn, vacs: List[Vacation]): ValidationResult[Unit] = {
    if (vacs.exists(v => isVacsOverlapped(v.since, v.until, vacIn.since, vacIn.until)))
      VacsMustNotOverlap.invalidNel
    else ().validNel
  }

  private def validatePeriodFromLastVac(vacIn: VacationIn, vacs: List[Vacation]): ValidationResult[Unit] = {
    val vacsBefore = vacs.filter { v => v.until.isBefore(vacIn.since) }
    val isEnoughDaysPassFromLastVac = vacsBefore.lastOption.map { lastVac =>
      val lastVacPeriod = lastVac.until.toEpochDay - lastVac.since.toEpochDay
      val daysFromLastVac = vacIn.since.toEpochDay - lastVac.until.toEpochDay
      daysFromLastVac >= lastVacPeriod
    }

    if (isEnoughDaysPassFromLastVac.getOrElse(true)) ().validNel
    else NotEnoughDaysPassFromLastVac.invalidNel
  }

  private def validatePeriodToNextVac(vacIn: VacationIn, vacs: List[Vacation]): ValidationResult[Unit] = {
    val vacsAfter = vacs.filter { v => v.since.isAfter(vacIn.until) }
    val isEnoughDaysToNextVac = vacsAfter.lastOption.map { nextVac =>
      val vacInPeriod = vacIn.until.toEpochDay - vacIn.since.toEpochDay
      val daysToNextVac = nextVac.since.toEpochDay - vacIn.until.toEpochDay
      daysToNextVac >= vacInPeriod
    }

    if (isEnoughDaysToNextVac.getOrElse(true)) ().validNel
    else NotEnoughDaysToNextVac.invalidNel
  }

  private def validateVacDaysTotalCountPerYear(vacIn: VacationIn, vacsCurrYear: List[Vacation]): ValidationResult[Unit] = {
    val vacsDaysCount = vacsCurrYear.map(vac => vac.until.toEpochDay - vac.since.toEpochDay).sum
    val remainedVacDaysCount = ValidationRules.vacDaysMaxCountPerYear - vacsDaysCount
    val vacInPeriod = vacIn.until.toEpochDay - vacIn.since.toEpochDay
    if (vacInPeriod >= remainedVacDaysCount) MaxCountVacDaysPerYearExceeded.invalidNel
    else ().validNel
  }

  private def validatePosIdOnVacRate(emplsOnVacWithSamePosIdCount: Int,
                                     emplsWithSamePosIdCount: Int): ValidationResult[Unit] = {

    val rate = (emplsOnVacWithSamePosIdCount.toFloat) / emplsWithSamePosIdCount
    if (rate <= ValidationRules.posIdOnVacRate) ().validNel
    else TooManyEmplsOfOnePosOnVac.invalidNel
  }

  def basicValidateVacIn(vacIn: VacationIn): Either[NonEmptyList[ServiceValidationError], VacationIn] = {
    (validateVacDirection(vacIn),
     validateVacOnlyInFuture(vacIn),
     validateVacPeriodWithin1Year(vacIn),
     validateMinVacPeriod(vacIn),
     validateMaxVacPeriod(vacIn)
    ).tupled.map(_ => vacIn).toEither
  }

  def validateVacInCreate(vacIn: VacationIn,
                          emplId: Long,
                          emplVacsCurrYear: List[Vacation],
                          overlappedVacsWithSamePosId: List[Vacation],
                          emplsWithSamePosId: List[Employee]): Either[NonEmptyList[ServiceValidationError], VacationIn] = {
    
    val emplsOnVacWithSamePosIdCount = overlappedVacsWithSamePosId
      .filter(_.employeeId != emplId)
      .groupBy(_.employeeId).size + 1

    val emplsWithSamePosIdCount = emplsWithSamePosId.length

    (
     validateVacNotOverlap(vacIn, emplVacsCurrYear),
     validatePeriodFromLastVac(vacIn, emplVacsCurrYear), 
     validatePeriodToNextVac(vacIn, emplVacsCurrYear),
     validateVacDaysTotalCountPerYear(vacIn, emplVacsCurrYear),
     validatePosIdOnVacRate(emplsOnVacWithSamePosIdCount, emplsWithSamePosIdCount)
    ).tupled.map(_ => vacIn).toEither
  }

  def validateVacInUpdate(vacIn: VacationIn,
                          emplId: Long,
                          vacId: Long,
                          emplVacsCurrYear: List[Vacation],
                          overlappedVacsWithSamePosId: List[Vacation],
                          emplsWithSamePosId: List[Employee]): Either[NonEmptyList[ServiceValidationError], VacationIn] = {

    val emplVacsCurrYearExcludeUpdVac = emplVacsCurrYear.filter(_.vacationId != vacId)

    val emplsOnVacWithSamePosIdCount = overlappedVacsWithSamePosId
      .filter(v => v.vacationId != vacId && v.employeeId != emplId)
      .groupBy(_.employeeId).size + 1
    
    val emplsWithSamePosIdCount = emplsWithSamePosId.length
    
    (
     validateVacNotOverlap(vacIn, emplVacsCurrYearExcludeUpdVac),
     validatePeriodFromLastVac(vacIn, emplVacsCurrYearExcludeUpdVac), 
     validatePeriodToNextVac(vacIn, emplVacsCurrYearExcludeUpdVac),
     validateVacDaysTotalCountPerYear(vacIn, emplVacsCurrYearExcludeUpdVac),
     validatePosIdOnVacRate(emplsOnVacWithSamePosIdCount, emplsWithSamePosIdCount)
    ).tupled.map(_ => vacIn).toEither
  }

  def checkVacIsChangeable(vac: Vacation): Either[ServiceValidationError, Vacation] = 
    if (vac.since.isAfter(LocalDate.now(clock))) Either.right(vac)
    else Either.left(CannotChangeOrDeleteNotFutureVac)
  
  def validateEmplIn(emplIn: EmployeeIn, foundPos: Option[Position]): Either[NonEmptyList[ServiceValidationError], EmployeeIn] = {
    (validateFirstName(emplIn.firstName), 
     validateLastName(emplIn.lastName), 
     validatePosIdExist(foundPos)
    ).mapN(EmployeeIn).toEither
  }

}