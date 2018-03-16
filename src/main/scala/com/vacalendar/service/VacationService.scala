package com.vacalendar.service

import cats.data._
import cats.implicits._
import cats.effect.Effect

import com.vacalendar.errors._
import com.vacalendar.domain._
import com.vacalendar.endpoints.QryParams.VacsQryParams
import com.vacalendar.validation.ServiceValidationAlgebra
import com.vacalendar.repository.{ VacationRepoAlgebra, EmployeeRepoAlgebra }

class VacationService[F[_]: Effect](vacRepo: VacationRepoAlgebra[F],
                                    emplRepo: EmployeeRepoAlgebra[F],
                                    V: ServiceValidationAlgebra) {
  
  def getVac(emplId: Long, vacId: Long): EitherT[F, AppError, Option[Vacation]] =
    for {
      optVac <- vacRepo.getVac(emplId, vacId)  
    } yield optVac

  def getVacs(emplId: Long, qryParams: VacsQryParams): EitherT[F, AppError, List[Vacation]] =
    for {
      optFoundEmpl <- emplRepo.getEmpl(emplId)

      _ <- EitherT
        .fromOption[F](optFoundEmpl, EmplNotFound)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)

      vacs <- vacRepo.getVacs(emplId, qryParams)

    } yield vacs

  def deleteVac(emplId: Long, vacId: Long): EitherT[F, AppError, Vacation] = {
    for {
      optVac <- vacRepo.getVac(emplId, vacId)      
      vac <- EitherT
        .fromOption[F](optVac, VacNotFound)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)
        
      _ <- EitherT.fromEither[F] { 
        V.checkVacIsChangeable(vac)
          .leftMap[AppError](AppError.ServiceValidationErrWrapper)
      }

      optDeletedVac <- vacRepo.deleteVac(emplId, vacId)
        
      deletedVac <- EitherT
        .fromOption[F](optDeletedVac, VacNotFound)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)

    } yield deletedVac
  }

  def createVac(emplId: Long, vacIn: VacationIn): EitherT[F, AppError, Vacation] = {
    for {
      optFoundEmpl <- emplRepo.getEmpl(emplId)      

      empl <- EitherT
        .fromOption[F](optFoundEmpl, EmplNotFound)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)

      validVacIn1 <- EitherT.fromEither[F] {
        V.basicValidateVacIn(vacIn)
          .leftMap[AppError](AppError.ServiceValidationErrsWrapper)
      }

      emplVacsCurrYear <- vacRepo.getEmplVacsCurrYear(emplId)
        
      overlappedVacsWithSamePosId <- vacRepo.getOverlappedPosIdVacs(empl.positionId, vacIn.since, vacIn.until)
        
      emplsWithSamePosId <- emplRepo.getEmplsByPosId(empl.positionId)
        
      validVacIn2 <- EitherT.fromEither[F] {
        V.validateVacInCreate(validVacIn1,
                              emplId,
                              emplVacsCurrYear, 
                              overlappedVacsWithSamePosId, 
                              emplsWithSamePosId)
        .leftMap[AppError](AppError.ServiceValidationErrsWrapper)
      }
      
      createdVac <- vacRepo.createVac(emplId, validVacIn2)

    } yield createdVac
  }

  def updateVac(emplId: Long, vacId: Long, vacIn: VacationIn): EitherT[F, AppError, Vacation] = {
    for {

      optFoundEmpl <- emplRepo.getEmpl(emplId)
      empl <- EitherT
        .fromOption[F](optFoundEmpl, EmplNotFound)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)
      
      optVac <- vacRepo.getVac(emplId, vacId)      
      vac <- EitherT
        .fromOption[F](optVac, VacNotFound)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)
      
      _ <- EitherT.fromEither[F] {
        V.checkVacIsChangeable(vac)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)
      }
        
      validVacIn1 <- EitherT.fromEither[F] { 
        V.basicValidateVacIn(vacIn)
        .leftMap[AppError](AppError.ServiceValidationErrsWrapper)
      }

      emplVacsCurrYear <- vacRepo.getEmplVacsCurrYear(empl.employeeId)
        
      overlappedVacsWithSamePosId <- vacRepo
        .getOverlappedPosIdVacs(empl.positionId, vacIn.since, vacIn.until)
        
      emplsWithSamePosId <- emplRepo.getEmplsByPosId(empl.positionId)
        
      validVacIn2 <- EitherT.fromEither[F] { 
        V.validateVacInUpdate(validVacIn1,
                              emplId,
                              vacId,
                              emplVacsCurrYear, 
                              overlappedVacsWithSamePosId, 
                              emplsWithSamePosId)
        .leftMap[AppError](AppError.ServiceValidationErrsWrapper)
      }
      
      optUpdatedVac <- vacRepo.updateVac(emplId, vacId, validVacIn2)

      updatedVac <- EitherT
        .fromOption[F](optUpdatedVac, VacNotFound)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)

    } yield updatedVac
  }
}