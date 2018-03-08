package com.vacalendar.repository

import cats.data._
import com.vacalendar.domain._
import com.vacalendar.errors._
import com.vacalendar.endpoints.QryParams.EmplSummariesQryParams

trait EmployeeSummaryRepoAlgebra[F[_]] {
  
  def getEmplSummary(emplId: Long): EitherT[F, AppError, Option[EmployeeSummary]]

  def getEmplSummaries(qryParams: EmplSummariesQryParams): EitherT[F, AppError, List[EmployeeSummary]]
}