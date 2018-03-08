package com.vacalendar.repository

import cats.data._

import com.vacalendar.errors._
import com.vacalendar.domain._
import com.vacalendar.endpoints.QryParams._

trait EmployeeRepoAlgebra[F[_]] {
  def createEmpl(emplIn: EmployeeIn): EitherT[F, AppError, Employee]

  def updateEmpl(emplId: Long, emplIn: EmployeeIn): EitherT[F, AppError, Option[Employee]]

  def getEmpls(qryParams: EmplsQryParams): EitherT[F, AppError, List[Employee]]

  def getEmpl(emplId: Long): EitherT[F, AppError, Option[Employee]]

  def deleteEmpl(emplId: Long): EitherT[F, AppError, Option[Employee]]

  def getEmplsByPosId(posId: Long): EitherT[F, AppError, List[Employee]]
}
