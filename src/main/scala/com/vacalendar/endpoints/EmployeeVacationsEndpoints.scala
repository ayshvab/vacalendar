package com.vacalendar.endpoints

import cats.data._
import cats.implicits._
import cats.effect.Effect

import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._

import org.http4s._
import org.http4s.dsl.Http4sDsl

import com.vacalendar.domain._
import com.vacalendar.errors._
import com.vacalendar.service.VacationService
import com.vacalendar.validation.QryParamsValidationAlgebra

class EmployeeVacationsEndpoints[F[_]: Effect](vacService: VacationService[F], 
                                               V: QryParamsValidationAlgebra,
                                               errHandler: EndpointErrorHandler[F]) extends Http4sDsl[F] {

  implicit val vacInDecoder = jsonOf[F, VacationIn]

  object OrderByQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("orderBy")
  object SinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("since")
  object UntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("until")

  def service: AuthedService[String, F] = 
    getEmplVacEndpoint <+>
    getEmplVacsEndpoint <+>
    deleteEmplVacEndpoint <+>
    createEmplVacEndpoint <+>
    updateEmplVacEndpoint

  private def getEmplVacsEndpoint: AuthedService[String, F] =  AuthedService {
    case GET -> Root / "employees" / LongVar(emplId) / "vacations" :?
      OrderByQueryParamMatcher(oBy) +&
      SinceQueryParamMatcher(since) +&
      UntilQueryParamMatcher(until) as _ => {

        val result = for {
        
          qryParams <- EitherT.fromEither[F] {
            V.validateAndPrepareVacsQryParams(oBy, since, until)
              .leftMap[AppError](AppError.QryParamsValidationErrsWrapper)
          }
              
          vacs <- vacService.getVacs(emplId, qryParams)
        } yield vacs

        result.value.flatMap {
          case Right(vacs) => Ok(vacs.asJson)
          case Left(e) => errHandler.handle(e)
        }
      }

    }

  private def getEmplVacEndpoint: AuthedService[String, F] =  AuthedService {
    case GET -> Root / "employees" / LongVar(emplId) / "vacations" / LongVar(vacId) as _ =>
      vacService.getVac(emplId, vacId).value.flatMap {
        case Right(found) => Ok(found.asJson)
        case Left(e) => errHandler.handle(e)
      }
  }

  private def deleteEmplVacEndpoint: AuthedService[String, F] =  AuthedService {
    case DELETE -> Root / "employees" / LongVar(emplId) / "vacations" / LongVar(vacId) as _ =>
      vacService.deleteVac(emplId, vacId).value.flatMap {
        case Right(_) => Ok()
        case Left(e) => errHandler.handle(e)
      }
  }

  private def createEmplVacEndpoint: AuthedService[String, F] =  AuthedService {
    case ar @ POST -> Root / "employees" / LongVar(emplId) / "vacations" as _ =>
      val action = for {
        vacIn <- ar.req.as[VacationIn]

        result <- vacService.createVac(emplId, vacIn).value
      } yield result

      action.flatMap {
        case Right(created) => Ok(created.asJson)
        case Left(e) => errHandler.handle(e)
      }
  }

  private def updateEmplVacEndpoint: AuthedService[String, F] =  AuthedService {
    case ar @ PATCH -> Root / "employees" / LongVar(emplId) / "vacations" / LongVar(vacId) as _ =>
      val action = for {
        vacIn <- ar.req.as[VacationIn]
        result <- vacService.updateVac(emplId, vacId, vacIn).value
      } yield  result

      action.flatMap {
        case Right(updated) => Ok(updated.asJson)
        case Left(e) => errHandler.handle(e)
      }
  }
    
}