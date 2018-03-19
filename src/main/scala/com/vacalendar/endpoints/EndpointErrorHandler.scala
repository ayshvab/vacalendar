package com.vacalendar.endpoints

import cats.Monad

import org.http4s.dsl.Http4sDsl
import org.http4s.Response

import io.circe.{ Encoder, Json }
import io.circe.syntax._

import com.vacalendar.errors._

class EndpointErrorHandler[F[_] : Monad] extends Http4sDsl[F] {

  implicit val encodeQryParamsValidationError: Encoder[QryParamsValidationError] = new Encoder[QryParamsValidationError] {
    final def apply(err: QryParamsValidationError): Json = Json.obj(
      ("code", err.code.asJson),
      ("message", err.message.asJson)
    )
  }

  implicit val encodeServiceValidationError: Encoder[ServiceValidationError] = new Encoder[ServiceValidationError] {
    final def apply(err: ServiceValidationError): Json = Json.obj(
      ("code", err.code.asJson),
      ("message", err.message.asJson)
    )
  }

  implicit val encodeServiceValidationErrorWrapper: Encoder[AppError.ServiceValidationErrWrapper] = 
    new Encoder[AppError.ServiceValidationErrWrapper] {
      final def apply(err: AppError.ServiceValidationErrWrapper): Json = Json.obj(("error", err.error.asJson))
    }

  implicit val encodeErrWrapper: Encoder[AppError] = new Encoder[AppError] {
    final def apply(errWrapper: AppError): Json = errWrapper match {

      case e@AppError.ServiceValidationErrWrapper(_) => e.asJson

      case AppError.QryParamsValidationErrsWrapper(errNel) => Json.obj(
        ("error", Json.obj(
            ("code", "QueryParamsValidationError".asJson),
            ("message", "Query params not valid".asJson),
            ("details", errNel.asJson(Encoder.encodeNonEmptyList(encodeQryParamsValidationError))) 
          )
        )
      )

      case AppError.ServiceValidationErrsWrapper(errNel) => Json.obj(
        ("error", Json.obj(
            ("code", "ServiceValidationErr".asJson),
            ("message", "Service has some validation errors".asJson),
            ("details", errNel.asJson(Encoder.encodeNonEmptyList(encodeServiceValidationError)))
          )
        )
      )

      case e => e.asJson
  
    }
  }

  val handle: AppError => F[Response[F]] = {
    case AppError.DbErrWrapper(_) => InternalServerError()
    case e@AppError.ServiceValidationErrWrapper(EmplNotFound) => NotFound(e.asJson.spaces2)
    case e@AppError.ServiceValidationErrWrapper(VacNotFound) => NotFound(e.asJson.spaces2)
    case e@AppError.ServiceValidationErrWrapper(PosNotFound) => NotFound(e.asJson.spaces2)
    case e => BadRequest(e.asJson.spaces2)
  }
}

