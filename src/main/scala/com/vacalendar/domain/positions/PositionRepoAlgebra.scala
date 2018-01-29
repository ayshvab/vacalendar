package com.vacalendar.domain.positions

trait PositionRepoAlgebra[F[_]] {

  def get(id: Long): F[Option[Position]]
}
