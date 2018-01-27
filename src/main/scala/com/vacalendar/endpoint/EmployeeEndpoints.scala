package com.vacalendar.endpoint

import cats.effect.Effect
import cats.implicits._
//import cats.data._

import com.vacalendar.domain.employees.{EmployeeService, EmployeeStatus}
//import io.circe._
//import io.circe.generic.auto._
import io.circe.generic.extras.semiauto._
//import io.circe.syntax._
//import org.http4s.circe._
import io.circe.Encoder
import org.http4s.HttpService
import org.http4s.dsl.Http4sDsl

object EmployeeEndpoints {
  def endpoints[F[_]: Effect](employeeService: EmployeeService[F]): HttpService[F] =
    new EmployeeEndpoints[F].endpoints(employeeService)
}

class EmployeeEndpoints[F[_]: Effect] extends Http4sDsl[F] {

  implicit val statusEncoder: Encoder[EmployeeStatus] = deriveEnumerationEncoder

  private def listEmployeesEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case GET -> Root / "employees" =>
        for {
//          retrived <- employeeService.list()
//          resp <- Ok(retrived.asJson)
          resp <-  Ok()
        } yield resp
    }

  def endpoints(employeeService: EmployeeService[F]): HttpService[F] =
    listEmployeesEndpoint(employeeService)
}
