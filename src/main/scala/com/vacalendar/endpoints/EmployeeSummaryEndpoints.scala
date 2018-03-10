package com.vacalendar.endpoints

import cats.data._
import cats.implicits._
import cats.effect.Effect

import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._

import org.http4s._
import org.http4s.dsl.Http4sDsl

import com.vacalendar.errors._
import com.vacalendar.service.EmployeeSummaryService
import com.vacalendar.validation.QryParamsValidationAlgebra

class EmployeeSummaryEndpoints[F[_]: Effect](emplSummaryService: EmployeeSummaryService[F], 
                                             V: QryParamsValidationAlgebra,
                                             errHandler: EndpointErrorHandler[F]) extends Http4sDsl[F] {

  object OrderByQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("orderBy")

  object FirstNameQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("firstname")
  object LastNameQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("lastname")
  object PositionIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("positionId")
  object PositionTitleQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("positionTitle")
  object VacationIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("vacationId")
  object EmployeeIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("employeeId")

  object SinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("since")
  object UntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("until")

  object OnVacQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("isOnVacacation")
  
  object CurrVacSinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("currVac.since")
  object CurrVacUntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("currVac.until")
  
  object PastVacsSinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("pastVacs.since")
  object PastVacsUntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("pastVacs.until")

  object FutureVacsSinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("futureVacs.since")
  object FutureVacsUntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("futureVacs.until")

  def service: AuthedService[String, F] = 
    getEmplSummaryEndpoind <+> 
    getEmplsSummariesEndpoint

  private def getEmplSummaryEndpoind: AuthedService[String, F] =  AuthedService {
      case GET -> Root / "employees" / "view=summary" / LongVar(emplId) as _ =>
        emplSummaryService.getEmplSummary(emplId).value.flatMap {
          case Right(summary) => Ok(summary.asJson)
          case Left(e) => errHandler.handle(e)
        }
    }

  private def getEmplsSummariesEndpoint: AuthedService[String, F] =  AuthedService {
      case GET -> Root / "employees" / "view=summary" :?
        OrderByQueryParamMatcher(oBy) +&
        EmployeeIdQueryParamMatcher(emplId) +&
        FirstNameQueryParamMatcher(fn) +&
        LastNameQueryParamMatcher(ln) +&
        PositionIdQueryParamMatcher(posId) +&
        PositionTitleQueryParamMatcher(posTitle) +&
        OnVacQueryParamMatcher(isOnVac) +&
        
        CurrVacSinceQueryParamMatcher(currVacSince) +&
        CurrVacUntilQueryParamMatcher(currVacUntil) +&
        
        PastVacsSinceQueryParamMatcher(pastVacsSince) +&
        PastVacsUntilQueryParamMatcher(pastVacsUntil) +&
        
        FutureVacsSinceQueryParamMatcher(futureVacsSince) +&
        FutureVacsUntilQueryParamMatcher(futureVacsUntil) as _ => {

          val result = for {
            qryParams <- EitherT.fromEither[F] {
              V.validateAndPrepareEmplSummariesQryParams(oBy = oBy,
                                                         emplId = emplId,
                                                         fn = fn,
                                                         ln = ln,
                                                         posId = posId,
                                                         posTitle = posTitle,
                                                         isOnVac = isOnVac,
                                                         
                                                         pastVacsSince = pastVacsSince,
                                                         pastVacsUntil = pastVacsUntil, 
                                                         currVacSince = currVacSince,
                                                         currVacUntil = currVacUntil,
                                                         
                                                         futureVacsSince = futureVacsSince,
                                                         futureVacsUntil = futureVacsUntil)
              .leftMap[AppError](AppError.QryParamsValidationErrsWrapper)
            }
            summaries <- emplSummaryService.getEmplSummaries(qryParams)
          } yield summaries

          result.value.flatMap {
            case Right(summaries) => Ok(summaries.asJson)
            case Left(e) => errHandler.handle(e)
          }
        }
    }
}