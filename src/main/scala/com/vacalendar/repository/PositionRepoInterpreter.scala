package com.vacalendar.repository

import cats.Monad
import com.vacalendar.domain.positions.{Position, PositionRepoAlgebra}
import doobie.implicits._
import doobie.util.transactor.Transactor


class PositionRepoInterpreter[F[_]: Monad](val xa: Transactor[F])
  extends PositionRepoAlgebra[F] {

  override def get(id: Long): F[Option[Position]] =
    sql"""SELECT position_id, title
          FROM positions
          WHERE position_id = $id
       """
      .query[Position]
      .option
      .transact(xa)
}

object PositionRepoInterpreter {
  def apply[F[_]: Monad](xa: Transactor[F]): PositionRepoInterpreter[F] =
    new PositionRepoInterpreter(xa)
}
