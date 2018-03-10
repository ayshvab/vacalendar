package com.vacalendar.endpoints.auth

import cats.effect._
import cats.implicits._

import tsec.jws.mac.JWSMacCV.genSigner
import tsec.jws.mac.JWTMac
import tsec.jwt.JWTClaims
import tsec.mac.imports.{ HMACSHA256, MacSigningKey }


object ApiTokenGenerator extends TokenGeneration[IO] {

  def gen(): IO[Unit] =
    for {
      _ <- IO(println("Generating API Token"))
      token <- tokenGenerator
      _ <- IO(println(token))
    } yield ()
}

class TokenGeneration[F[_]](implicit F: Sync[F]) {

  private val ApiToken = sys.env.get("VACALENDAR_API_TOKEN")
  private val ApiKey = sys.env.get("VACALENDAR_API_KEY")

  private def generateJwtKey(token: String): F[MacSigningKey[HMACSHA256]] = {
    F.catchNonFatal(HMACSHA256.buildKeyUnsafe(token.getBytes))
  }

  private def generateToken(claims: JWTClaims, jwtKey: MacSigningKey[HMACSHA256]): F[String] = 
    JWTMac.buildToString(claims, jwtKey)

  private val ifEmpty: F[String] = F.raiseError(new Exception("Api Token not found"))

  val tokenGenerator: F[String] = ApiToken.fold(ifEmpty) { apiToken =>
    for {
      jwtKey <- generateJwtKey(apiToken)
      claims = JWTClaims(subject = ApiKey, expiration = None)
      token <- generateToken(claims, jwtKey)
    } yield token
  }
}