package com.vacalendar.domain.employees

import cats._
import cats.data._
import cats.implicits._
import cats.data.Validated._
import com.vacalendar.domain.positions.PositionRepoAlgebra
import com.vacalendar.domain._

class EmployeeValidationInterpreter[F[_]: Monad](employeeRepo: EmployeeRepoAlgebra[F],
                                                 positionRepo: PositionRepoAlgebra[F])
  extends EmployeeValidationAlgebra[F] {

  type ValidationResult[A] = ValidatedNel[ValidationError, A]

  private def validateFirstName(firstName: String): ValidationResult[String] =
    if (firstName.matches("^[a-zA-Z]+$")) firstName.validNel else FirstNameHasSpecialCharactersError.invalidNel

  private def validateLastName(lastName: String): ValidationResult[String] =
    if (lastName.matches("^[a-zA-Z]+$")) lastName.validNel else LastNameHasSpecialCharactersError.invalidNel

  private def validatePositionId(positionId: Long): F[ValidationResult[Long]] =
    positionRepo.get(positionId: Long).map { found =>
      if (found.isEmpty) NonExistingPositionError.invalidNel
      else positionId.validNel
    }

  def checkEmployeeNew(employeeNew: EmployeeNew): EitherT[F, NonEmptyList[ValidationError], EmployeeNew] = EitherT {
    val validatedFirstName = validateFirstName(employeeNew.firstName)
    val validatedLastName = validateLastName(employeeNew.lastName)
    for {
      validatedPositionId <- validatePositionId(employeeNew.positionId)
    } yield (validatedFirstName, validatedLastName, validatedPositionId).mapN(EmployeeNew).toEither
  }

  private def validateStatus(status: String): ValidationResult[String] =
    Either.catchNonFatal(EmployeeStatus(status)) match {
      case Right(_) => status.validNel
      case Left(_) => NonExistingStatusError.invalidNel
    }


  private def validateEmployeeExistence(id: Long): F[ValidationResult[Employee]] =
    employeeRepo.get(id).map { found =>
      if (found.isEmpty) EmployeeNotFoundError.invalidNel
      else found.get.validNel
    }

  def checkEmployeeUpd(id: Long, employeeUpd: EmployeeUpd): EitherT[F, NonEmptyList[ValidationError], Employee] = EitherT {

    val validatedFirstName = validateFirstName(employeeUpd.firstName)
    val validatedLastName = validateLastName(employeeUpd.lastName)
    val validatedStatus = validateStatus(employeeUpd.status)

    for {
      validatedPositionId <- validatePositionId(employeeUpd.positionId)
      existingEmployee <- validateEmployeeExistence(id)
    } yield (
      existingEmployee,
      validatedFirstName,
      validatedLastName,
      validatedPositionId,
      validatedStatus
      ).mapN(Employee.update).toEither
  }

}

object EmployeeValidationInterpreter {
  def apply[F[_]: Monad](employeeRepo: EmployeeRepoAlgebra[F], positionRepo: PositionRepoAlgebra[F]) =
    new EmployeeValidationInterpreter[F](employeeRepo, positionRepo)
}
