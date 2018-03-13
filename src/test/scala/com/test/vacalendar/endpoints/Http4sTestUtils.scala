package com.test.vacalendar.endpoints

import cats._
import cats.data._
import cats.effect._

import org.http4s._
import org.http4s.server._

object Http4sTestUtils {

  private def authUser[F[_]](implicit F: Applicative[F]): Kleisli[OptionT[F, ?], Request[F], String] =
    Kleisli(_ => OptionT.liftF(F.pure("access_token")))

  def middleware[F[_]: Monad]: AuthMiddleware[F, String] = AuthMiddleware.apply[F, String](authUser)

  val ioMiddleware: AuthMiddleware[IO, String] = middleware[IO]
}