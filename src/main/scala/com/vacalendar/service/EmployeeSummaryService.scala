package com.vacalendar.service

import java.time.Clock
import cats.data._
import cats.effect.Effect

import com.vacalendar.domain._
import com.vacalendar.errors._
import com.vacalendar.endpoints.QryParams.EmplSummariesQryParams
import com.vacalendar.repository.EmployeeSummaryRepoAlgebra

class EmployeeSummaryService[F[_]: Effect](emplSummaryRepo: EmployeeSummaryRepoAlgebra[F]) {

  def getEmplSummary(emplId: Long, clock: Clock): EitherT[F, AppError, EmployeeSummary] = 
    for {
      optEmplSummary <- emplSummaryRepo.getEmplSummary(emplId, clock)
      emplSummary <- EitherT
        .fromOption[F](optEmplSummary, EmplNotFound)
        .leftMap[AppError](AppError.ServiceValidationErrWrapper)
    } yield emplSummary

  def getEmplSummaries(qryParams: EmplSummariesQryParams, clock: Clock): EitherT[F, AppError, List[EmployeeSummary]] = 
    for {
      emplSummaries <- emplSummaryRepo.getEmplSummaries(qryParams, clock)
    } yield emplSummaries
}