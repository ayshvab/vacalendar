package com.vacalendar.service

import java.time._
import cats.data._
import cats.implicits._
import cats.effect.Effect

import com.vacalendar.domain._
import com.vacalendar.errors._
import com.vacalendar.endpoints.QryParams._
import com.vacalendar.repository.{EmployeeRepoAlgebra, PositionRepoAlgebra}
import com.vacalendar.validation.ServiceValidationAlgebra

class EmployeeService[F[_]: Effect](emplRepo: EmployeeRepoAlgebra[F],
                                    posRepo: PositionRepoAlgebra[F],
                                    validation: ServiceValidationAlgebra) {

  def getEmpls(qryParams: EmplsQryParams): EitherT[F, AppError, List[Employee]] =
    for {
      empls <- emplRepo.getEmpls(qryParams)
    } yield empls

  def getEmpl(emplId: Long): EitherT[F, AppError, Option[Employee]] =
    for {
      foundEmpl <- emplRepo.getEmpl(emplId)
    } yield foundEmpl

  def deleteEmpl(emplId: Long): EitherT[F, AppError, Employee] = 
    for {
      optDeleted <- emplRepo.deleteEmpl(emplId)
        
      deleted <- EitherT
        .fromOption[F](optDeleted, EmplNotFound)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)

    } yield deleted

  def createEmpl(emplIn: EmployeeIn, clock: Clock): EitherT[F, AppError, Employee] = 
    for {
      optFoundPos <- posRepo.getPos(emplIn.positionId)
        
      validEmplIn <- EitherT.fromEither[F] {
        validation.validateEmplIn(emplIn, optFoundPos)
          .leftMap[AppError](AppError.ServiceValidationErrsWrapper)
      }

      createdEmpl <- emplRepo.createEmpl(validEmplIn, clock)

    } yield createdEmpl

  def updateEmpl(emplId: Long, emplIn: EmployeeIn, clock: Clock): EitherT[F, AppError, Employee] =
    for {
      foundPos <- posRepo.getPos(emplIn.positionId)
        
      validEmplIn <- EitherT.fromEither[F] {
        validation.validateEmplIn(emplIn, foundPos)
          .leftMap[AppError](AppError.ServiceValidationErrsWrapper)
      }
        
      optUpdatedEmpl <- emplRepo.updateEmpl(emplId, validEmplIn, clock)
        
      updatedEmpl <- EitherT
        .fromOption[F](optUpdatedEmpl, EmplNotFound)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)
      
    } yield updatedEmpl

}
