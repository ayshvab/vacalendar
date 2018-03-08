package com.vacalendar.repository

import java.time.LocalDate
import cats.data._

import com.vacalendar.errors._
import com.vacalendar.domain._
import com.vacalendar.endpoints.QryParams._

trait VacationRepoAlgebra[F[_]] {
  def getVac(emplId: Long, vacId: Long): EitherT[F, AppError, Option[Vacation]]

  def getVacs(emplId: Long, qryParams: VacsQryParams): EitherT[F, AppError, List[Vacation]]

  def getEmplVacsCurrYear(emplId: Long): EitherT[F, AppError, List[Vacation]]
  
  def getOverlappedPosIdVacs(posId: Long,
                             startVac: LocalDate,
                             endVac: LocalDate): EitherT[F, AppError, List[Vacation]]

  def deleteVac(emplId: Long, vacId: Long): EitherT[F, AppError, Option[Vacation]]

  def createVac(emplId: Long, vacNew: VacationIn): EitherT[F, AppError, Vacation]

  def updateVac(emplId: Long, vacId: Long, vacIn: VacationIn): EitherT[F, AppError, Option[Vacation]]
}