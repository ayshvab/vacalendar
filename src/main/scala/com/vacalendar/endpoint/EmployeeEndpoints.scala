package com.vacalendar.endpoint

import java.time.{Instant, LocalDate}
// import cats._
import cats.data._
import cats.implicits._
// import cats.data.Validated._
import cats.effect.Effect
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import io.circe.Encoder
import io.circe.Decoder
import org.http4s.HttpService
import org.http4s.dsl.Http4sDsl
import org.http4s._
import com.vacalendar.domain._
import com.vacalendar.domain.employees._
// import com.vacalendar.domain.{EmployeeNotFoundError, VacationNotFoundError, ValidationError}
import com.vacalendar.domain.employees.EmployeeIn
import com.vacalendar.domain.vacations.VacationIn
import com.vacalendar.domain.employees.EmployeeService

object EmployeeEndpoints {
  def endpoints[F[_]: Effect](employeeService: EmployeeService[F]): HttpService[F] =
    new EmployeeEndpoints[F].endpoints(employeeService)
}

class EmployeeEndpoints[F[_]: Effect] extends Http4sDsl[F] {

  implicit val encodeInstant: Encoder[Instant] = Encoder.encodeString.contramap[Instant](_.toString)

  implicit val encodeLocalDate: Encoder[LocalDate] = Encoder.encodeString.contramap[LocalDate](_.toString)

  implicit val decodeLocalDate: Decoder[LocalDate] = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(LocalDate.parse(str)).leftMap(t => "LocalDate")
  }

  implicit val employeeInDecoder = jsonOf[F, EmployeeIn]

  implicit val vacInDecoder = jsonOf[F, VacationIn]

  object OrderByQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("orderBy")

  object FirstNameQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("firstName")
  object LastNameQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("lastName")
  object PositionIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("positionId")
  object VacationIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("vacationId")
  object EmployeeIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("employeeId")

  object SinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("since")
  object SinceBeforeQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("sinceBefore")
  object SinceAfterQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("sinceAfter")

  object UntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("until")
  object UntilBeforeQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("untilBefore")
  object UntilAfterQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("untilAfter")

  private def createEmployeeEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case req @ POST -> Root / "employees" =>
        val action = for {
          employeeIn <- req.as[EmployeeIn]
          result <- employeeService.createEmployee(employeeIn).value
        } yield result

        action.flatMap {
          case Right(created) => Ok(created.asJson)
          case Left(errors: NonEmptyList[ValidationError]) => BadRequest(errors.asJson)
        }
    }

  private def updateEmployeeEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case req @ PATCH -> Root / "employees" / LongVar(employeeId) =>
        val action = for {
          employeeIn <- req.as[EmployeeIn]
          result <- employeeService.updateEmployee(employeeId, employeeIn).value
        } yield result

        action.flatMap {
          case Right(updated) => Ok(updated.asJson)
          case Left(errors: NonEmptyList[ValidationError]) => BadRequest(errors.asJson)
        }
    }

  private def deleteEmployeeEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case DELETE -> Root / "employees" / LongVar(id) =>
        employeeService.deleteEmployee(id).value.flatMap {
          case Right(_) => Ok()
          case Left(_) => NotFound("The employee was not found")
        }
    }

  private def listEmployeesEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case GET -> Root / "employees" :?
        OrderByQueryParamMatcher(oBy) +&
        FirstNameQueryParamMatcher(fn) +&
        LastNameQueryParamMatcher(ln) +&
        PositionIdQueryParamMatcher(posId) => {

          val result = for {
            qryParams <- EitherT[F, NonEmptyList[ValidationError], EmplsQryParams](QryParamsValidation.validateAndPrepareEmplsQryParams(oBy, fn, ln, posId).pure[F])
            employees <- EitherT.liftF[F, NonEmptyList[ValidationError], List[Employee]](employeeService.listEmployees(qryParams))
          } yield employees

          result.value.flatMap {
            case Right(employees) => Ok(employees.asJson)
            case Left(errors) => BadRequest(errors.asJson)
          }
      }
    }

  private def getEmployeeEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case GET -> Root / "employees" / LongVar(employeeId) =>
        employeeService.getEmployee(employeeId).value.flatMap {
          case Right(found) => Ok(found.asJson)
          case Left(EmployeeNotFoundError) => NotFound("The employee was not found")
        }
    }

  // ==> Vacations <==
  private def getEmployeeVacsEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case GET -> Root / "employees" / LongVar(employeeId) / "vacations" :?
        OrderByQueryParamMatcher(oBy) +&
        SinceBeforeQueryParamMatcher(sinceBefore) +&
        SinceQueryParamMatcher(since) +&
        SinceAfterQueryParamMatcher(sinceAfter) +&
        UntilBeforeQueryParamMatcher(untilBefore) +&
        UntilQueryParamMatcher(until) +&
        UntilAfterQueryParamMatcher(untilAfter) => {

          val result = for {
            qryParams <-  EitherT[F, NonEmptyList[ValidationError], VacsQryParams](
              QryParamsValidation.validateAndPrepareVacsQryParams(oBy,
                                                                 sinceBefore,
                                                                 since,
                                                                 sinceAfter,
                                                                 untilBefore,
                                                                 until,
                                                                 untilAfter).pure[F])

            vacs <- employeeService.getVacs(employeeId, qryParams)
          } yield vacs

          result.value.flatMap {
            case Right(vacs) => Ok(vacs.asJson)
            case Left(errors) => BadRequest(errors.asJson)
          }
        }

    }

  private def getEmployeeVacEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case GET -> Root / "employees" / LongVar(employeeId) / "vacations" / LongVar(vacationId) =>
        employeeService.getVac(employeeId, vacationId).value.flatMap {
          case Right(found) => Ok(found.asJson)
          case error@Left(VacationNotFoundError) => NotFound(error.asJson)
        }
    }

  private def deleteEmployeeVacEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case DELETE -> Root / "employees" / LongVar(employeeId) / "vacations" / LongVar(vacationId) =>
        employeeService.deleteVac(employeeId, vacationId).value.flatMap {
          case Right(_) => Ok()
          case Left(errors) => NotFound(errors.asJson)
        }
    }

  private def createEmployeeVacEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case req @ POST -> Root / "employees" / LongVar(employeeId) / "vacations" =>
        val action = for {
          vacIn <- req.as[VacationIn]

          result <- employeeService.createVac(employeeId, vacIn).value
        } yield result

        action.flatMap {
          case Right(created) => Ok(created.asJson)
          case Left(errors: NonEmptyList[ValidationError]) => BadRequest(errors.asJson)
        }
    }

  private def updateEmployeeVacEndpoint(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case req @ PATCH -> Root / "employees" / LongVar(employeeId) / "vacations" / LongVar(vacId) =>
        val action = for {
          vacIn <- req.as[VacationIn]
          result <- employeeService.updateVac(employeeId, vacId, vacIn).value
        } yield  result

        action.flatMap {
          case Right(created) => Ok(created.asJson)
          case Left(errors: NonEmptyList[ValidationError]) => BadRequest(errors.asJson)
        }
    }

  private def getEmployeeSummary(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case GET -> Root / "employees" / "view=summary" / LongVar(employeeId) =>
        employeeService.getEmployeeSummary(employeeId).value.flatMap {
          case Right(summary) => Ok(summary.asJson)
          case Left(EmployeeNotFoundError) => NotFound()
        }
    }

  private def getEmployeesSummaryList(employeeService: EmployeeService[F]): HttpService[F] =
    HttpService[F] {
      case GET -> Root / "employees" / "view=summary" =>
        for {
          employeesSummaryList <- employeeService.getEmployeesSummaryList()
          resp <- Ok(employeesSummaryList.asJson)
        } yield resp
    }

  def endpoints(employeeService: EmployeeService[F]): HttpService[F] =
    listEmployeesEndpoint(employeeService) <+>
      getEmployeeEndpoint(employeeService) <+>
      createEmployeeEndpoint(employeeService) <+>
      updateEmployeeEndpoint(employeeService) <+>
      deleteEmployeeEndpoint(employeeService) <+>
      getEmployeeVacEndpoint(employeeService) <+>
      getEmployeeVacsEndpoint(employeeService) <+>
      deleteEmployeeVacEndpoint(employeeService) <+>
      createEmployeeVacEndpoint(employeeService) <+>
      updateEmployeeVacEndpoint(employeeService) <+>
      getEmployeeSummary(employeeService) <+>
      getEmployeesSummaryList(employeeService)
}


case class EmplsQryParams(orderByParams: Option[OrderByParams] = None,
                          firstName: Option[String] = None,
                          lastName: Option[String] = None,
                          positionId: Option[Long] = None)

case class VacsQryParams(orderByParams: Option[OrderByParams] = None,
                         sinceBefore: Option[LocalDate] = None,
                         since: Option[LocalDate] = None,
                         sinceAfter: Option[LocalDate] = None,
                         untilBefore: Option[LocalDate] = None,
                         until: Option[LocalDate] = None,
                         untilAfter: Option[LocalDate] = None)

case class OrderByParams(field: String, asc: Boolean)

object QryParamsValidation {

  type ValidationResult[A] = ValidatedNel[ValidationError, A]

  val emplsOrderFieldsToDB = Map("employeeId" -> "employee_id",
                                 "positionId" -> "position_id",
                                 "firstName" -> "first_name",
                                 "lastName" -> "last_name",
                                 "created" -> "created",
                                 "updated" -> "updated")

  val vacsOrderFieldsToDB = Map("vacationId" -> "vacation_id",
                                "employeeId" -> "employee_id",
                                "since" -> "since",
                                "until" -> "until",
                                "created" -> "created",
                                "updated" -> "updated")

  private def validateOrderBy(rawOrderBy: Option[String], fieldsForOrder: Map[String, String]): ValidationResult[Option[OrderByParams]] = {
    val preparedField = rawOrderBy flatMap { s =>
      val normalized = s.stripPrefix("-")
      fieldsForOrder.get(normalized)
    }

    rawOrderBy match {
      case None => None.validNel
      case Some(s) if s.startsWith("-") => {
        if (preparedField.isDefined) Some(OrderByParams(preparedField.get, asc = false)).validNel
        else NotValidOrderByParamError.invalidNel
      }
      case Some(_) if (preparedField.isDefined) => Some(OrderByParams(preparedField.get, asc = true)).validNel
      case _ => NotValidOrderByParamError.invalidNel
    }
  }

  def validateAndPrepareEmplsQryParams(rawOrderBy: Option[String],
                                      firstName: Option[String],
                                      lastName: Option[String],
                                      positionId: Option[Long]): Either[NonEmptyList[ValidationError], EmplsQryParams] = {

    val result = validateOrderBy(rawOrderBy, emplsOrderFieldsToDB) map[EmplsQryParams]((o: Option[OrderByParams]) => o match {

      case Some(o) => EmplsQryParams(orderByParams = Some(o),
                                     firstName = firstName,
                                     lastName = lastName,
                                     positionId = positionId)

      case None => EmplsQryParams(firstName = firstName, lastName = lastName, positionId = positionId)
    })

    result.toEither
  }

  private def validateLocalDateStr(rawLocalDate: Option[String]): ValidationResult[Option[LocalDate]] = {
    rawLocalDate match {
      case None => None.validNel
      case Some(rld) => Either.catchNonFatal(LocalDate.parse(rld)) match {
        case Left(_) => LocalDateParseError.invalidNel
        case Right(ld) => Some(ld).validNel
      }
    }
  }

  def validateAndPrepareVacsQryParams(rawOrderBy: Option[String],
                                     sinceBefore: Option[String],
                                     since: Option[String],
                                     sinceAfter: Option[String],
                                     untilBefore: Option[String],
                                     until: Option[String],
                                     untilAfter: Option[String]): Either[NonEmptyList[ValidationError], VacsQryParams] = {
    
    val result = (validateOrderBy(rawOrderBy, vacsOrderFieldsToDB),
                  validateLocalDateStr(sinceBefore),
                  validateLocalDateStr(since),
                  validateLocalDateStr(sinceAfter),
                  validateLocalDateStr(untilBefore),
                  validateLocalDateStr(until),
                  validateLocalDateStr(untilAfter)) mapN(VacsQryParams)

    result.toEither
  }
}