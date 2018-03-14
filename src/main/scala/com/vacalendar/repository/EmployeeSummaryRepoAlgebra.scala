package com.vacalendar.repository

import java.time.Clock

import cats.data._
import com.vacalendar.domain._
import com.vacalendar.errors._
import com.vacalendar.endpoints.QryParams.EmplSummariesQryParams

trait EmployeeSummaryRepoAlgebra[F[_]] {
  
  def getEmplSummary(emplId: Long, clock: Clock = Clock.systemUTC()): EitherT[F, AppError, Option[EmployeeSummary]]

  def getEmplSummaries(qryParams: EmplSummariesQryParams = EmplSummariesQryParams(), 
                      clock: Clock = Clock.systemUTC()): EitherT[F, AppError, List[EmployeeSummary]]

}