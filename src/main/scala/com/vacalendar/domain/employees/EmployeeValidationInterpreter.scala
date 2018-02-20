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
  val maxTotalVacDaysCountPerY = 24
  val maxVacPeriod = 15
  val minVacPeriod = 2
  val posIdOnVacRate = 0.5
}

class EmployeeValidationInterpreter[F[_]: Monad](employeeRepo: EmployeeRepoAlgebra[F],
                                                 positionRepo: PositionRepoAlgebra[F])
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

  private def validateVacPeriodWithin1Y(vacIn: VacationIn): ValidationResult[Unit] =
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

  private def validateVacDaysTotalCountPerY(vacIn: VacationIn, currYVacs: List[Vacation]): ValidationResult[Unit] = {
    val vacsDaysCount = currYVacs.map(vac => vac.until.toEpochDay - vac.since.toEpochDay).sum
    val remainedVacDaysCount = ValidationRules.maxTotalVacDaysCountPerY - vacsDaysCount
    val vacInPeriod = vacIn.until.toEpochDay - vacIn.since.toEpochDay
    if (vacInPeriod >= remainedVacDaysCount) OutOfTotalVacationDaysPerYearError.invalidNel
    else ().validNel
  }

  private def validatePosIdOnVacRate(vacIn: VacationIn,
                                     employeesOnVacWithSamePosIdCount: Int,
                                     employeesWithSamePosIdCount: Int): ValidationResult[Unit] = {

    val rate = (employeesOnVacWithSamePosIdCount.toFloat + 1) / employeesWithSamePosIdCount
    val isRateOk = rate <= ValidationRules.posIdOnVacRate
    if (isRateOk) ().validNel
    else TooManyEmployeesOfOnePositionOnVacationError.invalidNel
  }

  private def checkVacIn(vacIn: VacationIn, employee: Employee): EitherT[F, NonEmptyList[ValidationError], VacationIn] = {
    val check1 = (
      validateVacDirection(vacIn),
      validateVacOnlyInFuture(vacIn),
      validateVacPeriodWithin1Y(vacIn),
      validateMinVacPeriod(vacIn),
      validateMaxVacPeriod(vacIn)
    ).mapN((_, _, _, _, _) => vacIn).toEither

    lazy val check2 = for {
      currYEmployeeVacs <- employeeRepo.getEmployeeVacsCurrY(vacIn.employeeId)

      overlappedVacsWithSamePosId <- employeeRepo.getEmployeeVacsOverlappedPosIdVacsForCurrY(
        employee.positionId, vacIn.since, vacIn.until)

      employeesWithSamePosId <- employeeRepo.getEmployeesByPosId(employee.positionId)
      
      employeesOnVacWithSamePosIdCount = overlappedVacsWithSamePosId.groupBy(_.employeeId).size
      employeesWithSamePosIdCount = employeesWithSamePosId.length
    } yield (
      validateVacNotOverlap(vacIn, currYEmployeeVacs),
      validatePeriodFromLastVac(vacIn, currYEmployeeVacs), 
      validatePeriodToNextVac(vacIn, currYEmployeeVacs),
      validateVacDaysTotalCountPerY(vacIn, currYEmployeeVacs),
      validatePosIdOnVacRate(vacIn, employeesOnVacWithSamePosIdCount, employeesWithSamePosIdCount)
    ).mapN((_, _, _, _, _) => vacIn).toEither

    for {
      _ <- EitherT.fromEither(check1)
      validVacIn <- EitherT(check2)
    } yield validVacIn
  }

  private def checkIdentityEmployeeIds(employeeIdFromUrl: Long,
                                       employeeIdFromVacationIn: Long): Either[NonEmptyList[ValidationError], Unit] = {
    if (employeeIdFromUrl != employeeIdFromVacationIn)
      Either.left[NonEmptyList[ValidationError], Unit](NonEmptyList.one(NotIdenticalEmployeeIdsError))
    else
      Either.right[NonEmptyList[ValidationError], Unit](())
  }

  private def checkEmployeeIn(employeeIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], EmployeeIn] = {
    val result = for {
      foundPos <- positionRepo.get(employeeIn.positionId: Long)

      validatedFirstName = validateFirstName(employeeIn.firstName)
      validatedLastName = validateLastName(employeeIn.lastName)
      validatedPosId = validatePosIdExist(foundPos)
    } yield (validatedFirstName, validatedLastName, validatedPosId).mapN(EmployeeIn).toEither

    EitherT(result)
  }

  def checkEmployeeExist(employeeId: Long): EitherT[F, NonEmptyList[ValidationError], Employee] = {
    val result = for {
      exist <- employeeRepo.getEmployee(employeeId).map { found =>
        if (found.isEmpty) Either.left[NonEmptyList[ValidationError], Employee](NonEmptyList.one(EmployeeNotFoundError))
        else Either.right[NonEmptyList[ValidationError], Employee](found.get)
      }
    } yield exist

    EitherT(result)
  }

  def checkCreateEmployee(employeeIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], EmployeeIn] =
    checkEmployeeIn(employeeIn)

  def checkUpdateEmployee(employeeId: Long, employeeIn: EmployeeIn): EitherT[F, NonEmptyList[ValidationError], EmployeeIn] =
    for {
      _ <- checkEmployeeExist(employeeId)
      checkedEmployeeIn <- checkEmployeeIn(employeeIn)
    } yield checkedEmployeeIn

  def checkCreateVac(employeeId: Long, vacIn: VacationIn): EitherT[F, NonEmptyList[ValidationError], VacationIn] =
    for {
      _ <- EitherT.fromEither { checkIdentityEmployeeIds(employeeId, vacIn.employeeId) }
      employee <- checkEmployeeExist(vacIn.employeeId)
      checkedVacIn <- checkVacIn(vacIn, employee)
    } yield checkedVacIn


  def checkUpdateVac(employeeId: Long, vacId: Long, vacIn: VacationIn): EitherT[F, NonEmptyList[ValidationError], VacationIn] = {
    for {
      _ <- EitherT.fromEither { checkIdentityEmployeeIds(employeeId, vacIn.employeeId) }
      _ <- checkIsChangeableVac(employeeId, vacId)
      employee <- checkEmployeeExist(vacIn.employeeId)
      validVacIn <- checkVacIn(vacIn, employee)
    } yield validVacIn
  }

  def checkIsChangeableVac(employeeId: Long, vacId: Long): EitherT[F, NonEmptyList[ValidationError], Unit] = {
    for {
      vacation <- EitherT.fromOptionF[F, NonEmptyList[ValidationError], Vacation](
        employeeRepo.getVac(employeeId, vacId), NonEmptyList.one(NotFoundError))
      _ <- EitherT.fromEither {
        if (LocalDate.now().isBefore(vacation.since)) Either.right[NonEmptyList[ValidationError], Unit](())
        else Either.left[NonEmptyList[ValidationError], Unit](NonEmptyList.one(CannotChangeOrDeleteCurrentOrFutureVacationsError))
      }
    } yield ()
  }
}

object EmployeeValidationInterpreter {
  def apply[F[_]: Monad](employeeRepo: EmployeeRepoAlgebra[F], positionRepo: PositionRepoAlgebra[F]) =
    new EmployeeValidationInterpreter[F](employeeRepo, positionRepo)
}
