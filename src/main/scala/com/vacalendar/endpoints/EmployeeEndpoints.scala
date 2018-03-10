package com.vacalendar.endpoints

import cats.data._
import cats.implicits._
import cats.effect.Effect

import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s._

import com.vacalendar.domain._
import com.vacalendar.errors._
import com.vacalendar.service.EmployeeService
import com.vacalendar.validation.QryParamsValidationAlgebra

class EmployeeEndpoints[F[_]: Effect](emplService: EmployeeService[F],
                                      V: QryParamsValidationAlgebra,
                                      errHandler: EndpointErrorHandler[F]) extends Http4sDsl[F] {

  implicit val employeeInDecoder = jsonOf[F, EmployeeIn]

  object OrderByQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("orderBy")
  object FirstNameQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("firstname")
  object LastNameQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("lastname")
  object PositionIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("positionId")

  def service: AuthedService[String, F] = 
    getEmplsEndpoint <+>
    getEmplEndpoint <+>
    createEmplEndpoint <+>
    updateEmplEndpoint <+>
    deleteEmplEndpoint

  private def createEmplEndpoint: AuthedService[String, F] = AuthedService {
      case ar@POST -> Root / "employees" as _ =>
        val action = for {
          emplIn <- ar.req.as[EmployeeIn]
          result <- emplService.createEmpl(emplIn).value
        } yield result

        action.flatMap {
          case Right(created) => Ok(created.asJson)
          case Left(e) => errHandler.handle(e)
        }
    }

  private def updateEmplEndpoint: AuthedService[String, F] = AuthedService {
      case ar @ PATCH -> Root / "employees" / LongVar(emplId) as _ =>
        val action = for {
          emplIn <- ar.req.as[EmployeeIn]
          result <- emplService.updateEmpl(emplId, emplIn).value
        } yield result

        action.flatMap {
          case Right(updated) => Ok(updated.asJson)
          case Left(e) => errHandler.handle(e)
        }
    }

  private def deleteEmplEndpoint: AuthedService[String, F] = AuthedService {
      case DELETE -> Root / "employees" / LongVar(emplId) as _ =>
        emplService.deleteEmpl(emplId).value.flatMap {
          case Right(_) => Ok()
          case Left(e) => errHandler.handle(e)
        }
    }

  private def getEmplsEndpoint: AuthedService[String, F] = AuthedService {
      case GET -> Root / "employees" :?
        OrderByQueryParamMatcher(oBy) +&
        FirstNameQueryParamMatcher(fn) +&
        LastNameQueryParamMatcher(ln) +&
        PositionIdQueryParamMatcher(posId) as _ => {

          val result = for {
            qryParams <- EitherT.fromEither[F] {
              V.validateAndPrepareEmplsQryParams(oBy, fn, ln, posId)
                .leftMap[AppError](AppError.QryParamsValidationErrsWrapper)
            }
            
            empls <- emplService.getEmpls(qryParams)
          } yield empls

          result.value.flatMap {
            case Right(empls) => Ok(empls.asJson)
            case Left(e) => errHandler.handle(e)
          }
      }
    }

  private def getEmplEndpoint: AuthedService[String, F] = AuthedService {
      case GET -> Root / "employees" / LongVar(emplId) as _ =>
        emplService.getEmpl(emplId).value.flatMap {
          case Right(found) => Ok(found.asJson)
          case Left(e) => errHandler.handle(e)
        }
    }
}



