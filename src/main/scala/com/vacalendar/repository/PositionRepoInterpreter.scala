package com.vacalendar.repository

import cats._
import cats.data._
import cats.implicits._

import doobie._
import doobie.implicits._
import doobie.util.transactor.Transactor

import com.vacalendar.domain._
import com.vacalendar.errors._

class PositionRepoInterpreter[F[_]](val xa: Transactor[F])
                                   (implicit F: MonadError[F, Throwable]) extends PositionRepoAlgebra[F] {

  def getPos(posId: Long): EitherT[F, AppError, Option[Position]] =
    PositionSQL.selectPos(posId).option
      .transact(xa)
      .attemptT
      .leftMap[AppError](AppError.DbErrWrapper)
}

object PositionSQL {
  def selectPos(posId: Long): Query0[Position] = 
    sql"""SELECT *
          FROM positions
          WHERE position_id = $posId
    """
    .query[Position]
}
