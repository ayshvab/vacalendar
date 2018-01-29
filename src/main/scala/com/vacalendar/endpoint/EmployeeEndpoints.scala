package com.vacalendar.endpoint

import java.time.Instant

import com.vacalendar.domain.{EmployeeNotFoundError, ValidationError}
import com.vacalendar.domain.employees.{EmployeeNew, EmployeeUpd}
//import com.vacalendar.domain.employees.Employee
//import cats._
import cats.data._
import cats.effect.Effect
import cats.implicits._

import com.vacalendar.domain.employees.{EmployeeService, EmployeeStatus}
import io.circe.generic.auto._
import io.circe.generic.extras.semiauto._
import io.circe.syntax._
import org.http4s.circe._
import io.circe.Encoder
//import io.circe.Decoder
import org.http4s.HttpService
import org.http4s.dsl.Http4sDsl

object EmployeeEndpoints {
  def endpoints[F[_]: Effect](employeeService: EmployeeService[F]): HttpService[F] =
    new EmployeeEndpoints[F].endpoints(employeeService)
}

class EmployeeEndpoints[F[_]: Effect] extends Http4sDsl[F] {

  implicit val encodeInstant: Encoder[Instant] = Encoder.encodeString.contramap[Instant](_.toString)

  // TODO encode/decode for LocalDate in vacation

  implicit val statusEncoder: Encoder[EmployeeStatus] = deriveEnumerationEncoder
//  implicit val employeeNewDecoder: Decoder[EmployeeNew] =
//    Decoder.forProduct3("firstName", "lastName", "positionId")(EmployeeNew.apply)

  implicit val employeeNewDecoder = jsonOf[F, EmployeeNew]

  implicit val employeeUpdDecoder = jsonOf[F, EmployeeUpd]

  private def createEmployeeEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case req @ POST -> Root / "employees" =>
        val action = for {
          employeeNew <- req.as[EmployeeNew]
          result <- employeeService.create(employeeNew).value
        } yield result

        action.flatMap {
          case Right(created) => Ok(created.asJson)
          case Left(errors: NonEmptyList[ValidationError]) => BadRequest(errors.asJson)
        }
    }

  private def updateEmployeeEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case req @ PATCH -> Root / "employees" / LongVar(id) =>
        val action = for {
          employeeUpd <- req.as[EmployeeUpd]
          result <- employeeService.update(id, employeeUpd).value
        } yield result

        action.flatMap {
          case Right(updated) => Ok(updated.asJson)
          case Left(errors: NonEmptyList[ValidationError]) => BadRequest(errors.asJson)
        }
    }

  private def deleteEmployeeEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case DELETE -> Root / "employees" / LongVar(id) =>
        employeeService.delete(id).value.flatMap {
          case Right(_) => Ok()
          case Left(_) => NotFound("The employee was not found")
        }
    }

  private def listEmployeesEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case GET -> Root / "employees" =>
        for {
          employees <- employeeService.list()
          resp <- Ok(employees.asJson)
        } yield resp
    }

  private def getEmployeeEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case GET -> Root / "employees" / LongVar(id) =>
        employeeService.get(id).value.flatMap {
          case Right(found) => Ok(found.asJson)
          case Left(EmployeeNotFoundError) => NotFound("The employee was not found")
        }
    }


  def endpoints(employeeService: EmployeeService[F]): HttpService[F] =
    listEmployeesEndpoint(employeeService) <+>
      getEmployeeEndpoint(employeeService) <+>
      createEmployeeEndpoint(employeeService) <+>
      updateEmployeeEndpoint(employeeService) <+>
      deleteEmployeeEndpoint(employeeService)
}
