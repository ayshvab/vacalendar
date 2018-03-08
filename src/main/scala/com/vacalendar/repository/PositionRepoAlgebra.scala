package com.vacalendar.repository

import cats.data._
import com.vacalendar.domain._
import com.vacalendar.errors._

trait PositionRepoAlgebra[F[_]] {

  def getPos(posId: Long): EitherT[F, AppError, Option[Position]]
}
