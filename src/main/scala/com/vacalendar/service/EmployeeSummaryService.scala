package com.vacalendar.service

import cats.data._
import cats.effect.Effect

import com.vacalendar.domain._
import com.vacalendar.errors._
import com.vacalendar.endpoints.QryParams.EmplSummariesQryParams
import com.vacalendar.repository.EmployeeSummaryRepoAlgebra

class EmployeeSummaryService[F[_]: Effect](emplSummaryRepo: EmployeeSummaryRepoAlgebra[F]) {

  def getEmplSummary(emplId: Long): EitherT[F, AppError, Option[EmployeeSummary]] = 
    for {
      foundEmplSummary <- emplSummaryRepo.getEmplSummary(emplId)
    } yield foundEmplSummary

  def getEmplSummaries(qryParams: EmplSummariesQryParams): EitherT[F, AppError, List[EmployeeSummary]] = 
    for {
      emplSummaries <- emplSummaryRepo.getEmplSummaries(qryParams)
    } yield emplSummaries
}