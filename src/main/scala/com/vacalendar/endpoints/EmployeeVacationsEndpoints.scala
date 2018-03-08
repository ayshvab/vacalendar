package com.vacalendar.endpoints

import cats.data._
import cats.implicits._
import cats.effect.Effect

import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._

import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.HttpService

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

  def service: HttpService[F] = 
    getEmplVacEndpoint <+>
    getEmplVacsEndpoint <+>
    deleteEmplVacEndpoint <+>
    createEmplVacEndpoint <+>
    updateEmplVacEndpoint

  private def getEmplVacsEndpoint: HttpService[F] = HttpService[F] {
    case GET -> Root / "employees" / LongVar(emplId) / "vacations" :?
      OrderByQueryParamMatcher(oBy) +&
      SinceQueryParamMatcher(since) +&
      UntilQueryParamMatcher(until) => {

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

  private def getEmplVacEndpoint: HttpService[F] = HttpService[F] {
    case GET -> Root / "employees" / LongVar(emplId) / "vacations" / LongVar(vacId) =>
      vacService.getVac(emplId, vacId).value.flatMap {
        case Right(found) => Ok(found.asJson)
        case Left(e) => errHandler.handle(e)
      }
  }

  private def deleteEmplVacEndpoint: HttpService[F] = HttpService[F] {
    case DELETE -> Root / "employees" / LongVar(emplId) / "vacations" / LongVar(vacId) =>
      vacService.deleteVac(emplId, vacId).value.flatMap {
        case Right(_) => Ok()
        case Left(e) => errHandler.handle(e)
      }
  }

  private def createEmplVacEndpoint: HttpService[F] = HttpService[F] {
    case req @ POST -> Root / "employees" / LongVar(emplId) / "vacations" =>
      val action = for {
        vacIn <- req.as[VacationIn]

        result <- vacService.createVac(emplId, vacIn).value
      } yield result

      action.flatMap {
        case Right(created) => Ok(created.asJson)
        case Left(e) => errHandler.handle(e)
      }
  }

  private def updateEmplVacEndpoint: HttpService[F] = HttpService[F] {
    case req @ PATCH -> Root / "employees" / LongVar(emplId) / "vacations" / LongVar(vacId) =>
      val action = for {
        vacIn <- req.as[VacationIn]
        result <- vacService.updateVac(emplId, vacId, vacIn).value
      } yield  result

      action.flatMap {
        case Right(updated) => Ok(updated.asJson)
        case Left(e) => errHandler.handle(e)
      }
  }
    
}