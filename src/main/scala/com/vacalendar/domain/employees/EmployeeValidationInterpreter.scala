package com.vacalendar.domain.employees

import java.time.LocalDate

import cats._
import cats.data._
import cats.implicits._
import cats.data.Validated._
import com.vacalendar.domain.positions.{Position, PositionRepoAlgebra}
import com.vacalendar.domain._
import com.vacalendar.domain.vacations.{Vacation, VacationIn}

object ValidationRules {
  val vacDaysMaxCountPerYear = 24
  val maxVacPeriod = 15
  val minVacPeriod = 2
  val posIdOnVacRate = 0.5
}

class EmployeeValidationInterpreter[F[_]: Monad](emplRepo: EmployeeRepoAlgebra[F],
                                                 posRepo: PositionRepoAlgebra[F])
  extends EmployeeValidationAlgebra[F] {

  type ValidationResult[A] = ValidatedNel[ValidationError, A]

  private def isVacsOverlapped(startVac1: LocalDate,
                               endVac1: LocalDate,
                               startVac2: LocalDate,
                               endVac2: LocalDate): Boolean = {

    (startVac1.isBefore(endVac2) || startVac1.isEqual(endVac2)) &&
      (startVac2.isBefore(endVac1) || startVac2.isEqual(endVac1))
  }

  private def validateFirstName(firstName: String): ValidationResult[String] =
    if (firstName.matches("^[a-zA-Z]+$")) firstName.validNel
    else FirstNameHasSpecialCharactersError.invalidNel

  private def validateLastName(lastName: String): ValidationResult[String] =
    if (lastName.matches("^[a-zA-Z]+$")) lastName.validNel
    else LastNameHasSpecialCharactersError.invalidNel

  private def validatePosIdExist(foundPos: Option[Position]): ValidationResult[Long] =
    if (foundPos.isEmpty) NonExistingPositionError.invalidNel
    else foundPos.get.positionId.validNel

  private def validateVacDirection(vacIn: VacationIn): ValidationResult[Unit] =
    if (vacIn.since.isBefore(vacIn.until)) ().validNel
    else VacationMustStartBeforeUntilError.invalidNel

  private def validateVacPeriodWithin1Year(vacIn: VacationIn): ValidationResult[Unit] =
    if (vacIn.since.getYear == vacIn.until.getYear) ().validNel
    else VacationMustStartAndEndOnOneYear.invalidNel

  private def validateMinVacPeriod(vacIn: VacationIn): ValidationResult[Unit] = {
    val vacPeriod = vacIn.until.toEpochDay - vacIn.since.toEpochDay
    if (vacPeriod < ValidationRules.minVacPeriod) VacationPeriodOutOfMinBoundError.invalidNel
    else ().validNel
  }

  private def validateMaxVacPeriod(vacIn: VacationIn): ValidationResult[Unit] = {
    val vacPeriod = vacIn.until.toEpochDay - vacIn.since.toEpochDay
    if (vacPeriod > ValidationRules.maxVacPeriod) VacationPeriodOutOfMaxBoundError.invalidNel
    else ().validNel
  }

  private def validateVacOnlyInFuture(vacIn: VacationIn): ValidationResult[Unit] = {
    if (vacIn.since.isAfter(LocalDate.now())) ().validNel
    else VacataionMustBeOnlyInFutureError.invalidNel
  }

  private def validateVacNotOverlap(vacIn: VacationIn, vacs: List[Vacation]): ValidationResult[Unit] = {
    if (vacs.exists(v => isVacsOverlapped(v.since, v.until, vacIn.since, vacIn.until)))
      VacationMustNotOverlappedError.invalidNel
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
    else NotEnoughDaysPassFromLastVacationError.invalidNel
  }

  private def validatePeriodToNextVac(vacIn: VacationIn, vacs: List[Vacation]): ValidationResult[Unit] = {
    val vacsAfter = vacs.filter { v => v.since.isAfter(vacIn.until) }
    val isEnoughDaysToNextVac = vacsAfter.lastOption.map { nextVac =>
      val vacInPeriod = vacIn.until.toEpochDay - vacIn.since.toEpochDay
      val daysToNextVac = nextVac.since.toEpochDay - vacIn.until.toEpochDay
      daysToNextVac >= vacInPeriod
    }

    if (isEnoughDaysToNextVac.getOrElse(true)) ().validNel
    else NotEnoughDaysToNextVacationError.invalidNel
  }

  private def validateVacDaysTotalCountPerYear(vacIn: VacationIn, vacsCurrYear: List[Vacation]): ValidationResult[Unit] = {
    val vacsDaysCount = vacsCurrYear.map(vac => vac.until.toEpochDay - vac.since.toEpochDay).sum
    val remainedVacDaysCount = ValidationRules.vacDaysMaxCountPerYear - vacsDaysCount
    val vacInPeriod = vacIn.until.toEpochDay - vacIn.since.toEpochDay
    if (vacInPeriod >= remainedVacDaysCount) OutOfTotalVacationDaysPerYearError.invalidNel
    else ().validNel
  }

  private def validatePosIdOnVacRate(emplsOnVacWithSamePosIdCount: Int,
                                     emplsWithSamePosIdCount: Int): ValidationResult[Unit] = {

    val rate = (emplsOnVacWithSamePosIdCount.toFloat) / emplsWithSamePosIdCount
    if (rate <= ValidationRules.posIdOnVacRate) ().validNel
    else TooManyEmployeesOfOnePositionOnVacationError.invalidNel
  }

  def basicValidateVacIn(vacIn: VacationIn): Either[NonEmptyList[ValidationError], VacationIn] = {
    (validateVacDirection(vacIn),
     validateVacOnlyInFuture(vacIn),
     validateVacPeriodWithin1Year(vacIn),
     validateMinVacPeriod(vacIn),
     validateMaxVacPeriod(vacIn)
    ).tupled.map((_) => vacIn).toEither
  }

  def validateVacInCreate(vacIn: VacationIn, empl: Employee): EitherT[F, NonEmptyList[ValidationError], VacationIn] = {
    val res = for {
      emplVacsCurrYear <- emplRepo.getEmplVacsCurrYear(empl.employeeId)

      overlappedVacsWithSamePosId <- emplRepo
        .getOverlappedPosIdVacs(empl.positionId, vacIn.since, vacIn.until)

      emplsWithSamePosId <- emplRepo.getEmplsByPosId(empl.positionId)

      emplsOnVacWithSamePosIdCount = overlappedVacsWithSamePosId.groupBy(_.employeeId).size + 1
      emplsWithSamePosIdCount = emplsWithSamePosId.length
    } yield (
      validateVacNotOverlap(vacIn, emplVacsCurrYear),
      validatePeriodFromLastVac(vacIn, emplVacsCurrYear), 
      validatePeriodToNextVac(vacIn, emplVacsCurrYear),
      validateVacDaysTotalCountPerYear(vacIn, emplVacsCurrYear),
      validatePosIdOnVacRate(emplsOnVacWithSamePosIdCount, emplsWithSamePosIdCount)
    ).tupled.map((_) => vacIn).toEither

    EitherT(res)
  }

  def validateVacInUpdate(vacId: Long, vacIn: VacationIn, empl: Employee): EitherT[F, NonEmptyList[ValidationError], VacationIn] = {

    val res = for {
      emplVacsCurrYear <- emplRepo
        .getEmplVacsCurrYear(empl.employeeId)
        .map(_.filter(v => v.vacationId != vacId))
  
      overlappedVacsWithSamePosId <- emplRepo
        .getOverlappedPosIdVacs(empl.positionId, vacIn.since, vacIn.until)
        .map(_.filter(v => v.vacationId != vacId))
        
      emplsWithSamePosId <- emplRepo.getEmplsByPosId(empl.positionId)
      
      emplsOnVacWithSamePosIdCount = overlappedVacsWithSamePosId.groupBy(_.employeeId).size + 1
      emplsWithSamePosIdCount = emplsWithSamePosId.length
    } yield (
      validateVacNotOverlap(vacIn, emplVacsCurrYear),
      validatePeriodFromLastVac(vacIn, emplVacsCurrYear), 
      validatePeriodToNextVac(vacIn, emplVacsCurrYear),
      validateVacDaysTotalCountPerYear(vacIn, emplVacsCurrYear),
      validatePosIdOnVacRate(emplsOnVacWithSamePosIdCount, emplsWithSamePosIdCount)
    ).tupled.map((_) => vacIn).toEither

    EitherT(res)
  }

  def checkVacIsChangeable(emplId: Long, vacId: Long): EitherT[F, NonEmptyList[ValidationError], Unit] = {
    for {
      vac <- EitherT.fromOptionF[F, NonEmptyList[ValidationError], Vacation](
        emplRepo.getVac(emplId, vacId), NonEmptyList.one(NotFoundError))
      _ <- EitherT.fromEither {
        if (LocalDate.now().isBefore(vac.since)) Either.right[NonEmptyList[ValidationError], Unit](())
        else Either.left[NonEmptyList[ValidationError], Unit](NonEmptyList.one(CannotChangeOrDeleteCurrentOrFutureVacationsError))
      }
    } yield ()
  }

  def checkEmplExist(employeeId: Long): EitherT[F, NonEmptyList[ValidationError], Employee] = {
    val result = for {
      exist <- emplRepo.getEmployee(employeeId).map { found =>
        if (found.isEmpty) Either.left[NonEmptyList[ValidationError], Employee](NonEmptyList.one(EmployeeNotFoundError))
        else Either.right[NonEmptyList[ValidationError], Employee](found.get)
      }
    } yield exist

    EitherT(result)
  }

  def validateEmplIn(emplIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], EmployeeIn] = {
    val result = for {
      foundPos <- posRepo.get(emplIn.positionId: Long)

      validatedFirstName = validateFirstName(emplIn.firstName)
      validatedLastName = validateLastName(emplIn.lastName)
      validatedPosId = validatePosIdExist(foundPos)
    } yield (validatedFirstName, validatedLastName, validatedPosId).mapN(EmployeeIn).toEither

    EitherT(result)
  }

}

object EmployeeValidationInterpreter {
  def apply[F[_]: Monad](emplRepo: EmployeeRepoAlgebra[F], posRepo: PositionRepoAlgebra[F]) =
    new EmployeeValidationInterpreter[F](emplRepo, posRepo)
}
