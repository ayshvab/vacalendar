package com.vacalendar.domain.employees

import cats._
import cats.data._
//import cats.implicits._
import cats.syntax.all._
import com.vacalendar.domain.{EmployeeNotFoundError, ValidationError}

class EmployeeService[F[_]](repo: EmployeeRepoAlgebra[F],
                            validation: EmployeeValidationAlgebra[F]) {

  def list(): F[List[Employee]] = repo.list()

  def get(id: Long)(implicit M: Monad[F]): EitherT[F, EmployeeNotFoundError.type, Employee] =
    EitherT.fromOptionF(repo.get(id), EmployeeNotFoundError)

  def delete(id: Long)(implicit M: Monad[F]): EitherT[F, EmployeeNotFoundError.type, Unit] = EitherT {
    repo.delete(id).map(deletedCount => if (deletedCount == 0) Left(EmployeeNotFoundError) else Right(()))
  }

  def create(employeeNew: EmployeeNew)(implicit M: Monad[F]): EitherT[F, NonEmptyList[ValidationError], Employee] =
    for {
      validatedEmployeeNew <- validation.checkEmployeeNew(employeeNew)
      createdEmployee <- EitherT.liftF(repo.create(validatedEmployeeNew))
    } yield createdEmployee

  def update(id: Long, employeeUpd: EmployeeUpd)(implicit M: Monad[F]): EitherT[F, NonEmptyList[ValidationError], Employee] =
    for {
      validEmployeeWithUpd <- validation.checkEmployeeUpd(id, employeeUpd)
      updatedEmployee <- EitherT.liftF(repo.update(id, validEmployeeWithUpd))
    } yield updatedEmployee


}

object EmployeeService {
  def apply[F[_]](repo: EmployeeRepoAlgebra[F], validation: EmployeeValidationAlgebra[F]) =
    new EmployeeService[F](repo, validation)
}
