package com.vacalendar.domain.employees

import cats._

class EmployeeService[F[_]](repo: EmployeeRepoAlgebra[F]) {
//  import cats.syntax.all._

  def list(): F[List[Employee]] = repo.list()
}

object EmployeeService {
  def apply[F[_]: Monad](repo: EmployeeRepoAlgebra[F]) = new EmployeeService[F](repo)
}
