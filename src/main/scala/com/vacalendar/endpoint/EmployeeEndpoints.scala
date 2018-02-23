package com.vacalendar.endpoint

import java.time.{Instant, LocalDate}
import cats.data._
import cats.implicits._
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

  object FirstNameQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("firstname")
  object LastNameQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("lastname")
  object PositionIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("positionId")
  object PositionTitleQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("positionTitle")
  object VacationIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("vacationId")
  object EmployeeIdQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Long]("employeeId")

  object SinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("since")
  object UntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("until")

  object OnVacQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("isOnVacation")
  
  object CurrVacSinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("currentVacation.since")
  object CurrVacUntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("currentVacation.until")
  
  object PastVacsSinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("pastVacations.since")
  object PastVacsUntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("pastVacations.until")

  object FutureVacsSinceQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("futureVacations.since")
  object FutureVacsUntilQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("futureVacations.until")

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
            qryParams <- EitherT[F, NonEmptyList[ValidationError], EmplsQryParams](
              QryParamsValidation.validateAndPrepareEmplsQryParams(oBy, fn, ln, posId).pure[F])
            employees <- EitherT.liftF[F, NonEmptyList[ValidationError], List[Employee]](
              employeeService.listEmployees(qryParams))
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
        SinceQueryParamMatcher(since) +&
        UntilQueryParamMatcher(until) => {

          val result = for {
            qryParams <-  EitherT[F, NonEmptyList[ValidationError], VacsQryParams](
              QryParamsValidation.validateAndPrepareVacsQryParams(oBy, since, until).pure[F])

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
        FutureVacsUntilQueryParamMatcher(futureVacsUntil) => {

          val result = for {
            qryParams <- EitherT[F, NonEmptyList[ValidationError], SumrsQryParams](
              QryParamsValidation.validateAndPrepareSumrsQryParams(oBy = oBy,
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
                                                                   futureVacsUntil = futureVacsUntil).pure[F])
            sumrs <- EitherT.liftF[F, NonEmptyList[ValidationError], List[EmployeeSummary]](employeeService.getEmployeesSummaryList(qryParams))
          } yield sumrs

          result.value.flatMap {
            case Right(sumrs) => Ok(sumrs.asJson)
            case Left(errors) => BadRequest(errors.asJson)
          }
        }
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
                         since: Option[DateParams] = None,
                         until: Option[DateParams] = None )

case class SumrsQryParams(orderByParams: Option[OrderByParams] = None,

                          employeeId: Option[Long] = None,
                          firstName: Option[String] = None,
                          lastName: Option[String] = None,
                          positionId: Option[Long] = None,
                          positionTitle: Option[String] = None,
                          isOnVac: Option[Boolean] = None,
                          
                          pastVacsSince: Option[DateParams] = None,
                          pastVacsUntil: Option[DateParams] = None,
                          
                          currVacSince: Option[DateParams] = None,
                          currVacUntil: Option[DateParams] = None,
                          
                          futureVacsSince: Option[DateParams] = None,
                          futureVacsUntil: Option[DateParams] = None)

case class OrderByParams(field: String, asc: Boolean)

case class DateParams(before: Option[LocalDate] = None, 
                      exact: Option[LocalDate] = None, 
                      after: Option[LocalDate] = None)

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

  val sumrsOrderFields = List("employeeId", "firstName", "lastName", "positionId", "remainedVacationDays")
  val sumrsOrderFieldsMap = sumrsOrderFields.zip(sumrsOrderFields).toMap

  private def validateAndPrepareOrderBy(rawOrderBy: Option[String], fieldsForOrder: Map[String, String]): ValidationResult[Option[OrderByParams]] = {
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

  def validateAndPrepareEmplsQryParams(oBy: Option[String],
                                      fn: Option[String],
                                      ln: Option[String],
                                      posId: Option[Long]): Either[NonEmptyList[ValidationError], EmplsQryParams] = {

    val result = (validateAndPrepareOrderBy(oBy, emplsOrderFieldsToDB),
                  fn.validNel[ValidationError],
                  ln.validNel[ValidationError],
                  posId.validNel[ValidationError]) mapN (EmplsQryParams.apply)

    result.toEither
  }

  private def validateAndPrepareLocalDateStr(raw: Option[String]): ValidationResult[Option[DateParams]] = {
    val prefix = "_.."
    val suffix = ".._"
    val sep = Array('.', '.')
    raw match {
      case None => None.validNel
      case Some("_.._") => None.validNel
      case Some(s) if (s.startsWith(prefix)) => Either.catchNonFatal(LocalDate.parse(s.stripPrefix(prefix))) match {
        case Left(_) => LocalDateParseError.invalidNel
        case Right(b@_) => Some(DateParams(before = Some(b))).validNel
      }
      case Some(s) if (s.endsWith(suffix)) => Either.catchNonFatal(LocalDate.parse(s.stripSuffix(suffix))) match {
        case Left(_) => LocalDateParseError.invalidNel
        case Right(a@_) => Some(DateParams(after = Some(a))).validNel
      }
      case Some(s) if (s.contains(sep)) => {
        Either.catchNonFatal {
            val Array(a, b) = s.split(sep).filter(_.nonEmpty)
            DateParams(after = Some(LocalDate.parse(a)), before = Some(LocalDate.parse(b)))
        } match {
            case Left(_) => LocalDateParseError.invalidNel
            case Right(dateParams) => Some(dateParams).validNel
        }
      }
      case Some(s) => Either.catchNonFatal(LocalDate.parse(s)) match {
        case Left(_) => LocalDateParseError.invalidNel
        case Right(date) => Some(DateParams(exact = Some(date))).validNel
      }
      case _ => LocalDateParseError.invalidNel
    }
  }

  private def validateAndPrepareIsOnVac(raw: Option[String]): ValidationResult[Option[Boolean]] = {
    raw match {
      case None => None.validNel
      case Some("t") => Some(true).validNel
      case Some("f") => Some(false).validNel
      case Some(_) => NotValidQueryParamError.invalidNel
    }
  }

  def validateAndPrepareVacsQryParams(rawOrderBy: Option[String],
                                      since: Option[String],
                                      until: Option[String]): Either[NonEmptyList[ValidationError], VacsQryParams] = {

    val result = (validateAndPrepareOrderBy(rawOrderBy, vacsOrderFieldsToDB),
                  validateAndPrepareLocalDateStr(since),
                  validateAndPrepareLocalDateStr(until)) mapN(VacsQryParams)

    result.toEither
  }

  def validateAndPrepareSumrsQryParams(oBy: Option[String],
                                       emplId: Option[Long],
                                       fn: Option[String],
                                       ln: Option[String],
                                       posId: Option[Long],
                                       posTitle: Option[String],
                                       isOnVac: Option[String],
                                       
                                       pastVacsSince: Option[String],
                                       pastVacsUntil: Option[String],
                                       
                                       currVacSince: Option[String],
                                       currVacUntil: Option[String],
                                       
                                       futureVacsSince: Option[String],
                                       futureVacsUntil: Option[String]): Either[NonEmptyList[ValidationError], SumrsQryParams] = {

    val result = (validateAndPrepareOrderBy(oBy, sumrsOrderFieldsMap),
                  emplId.validNel[ValidationError],
                  fn.validNel[ValidationError],
                  ln.validNel[ValidationError],
                  posId.validNel[ValidationError],
                  posTitle.validNel[ValidationError],

                  validateAndPrepareIsOnVac(isOnVac),

                  validateAndPrepareLocalDateStr(pastVacsSince),
                  validateAndPrepareLocalDateStr(pastVacsUntil),

                  validateAndPrepareLocalDateStr(currVacSince),
                  validateAndPrepareLocalDateStr(currVacUntil),

                  validateAndPrepareLocalDateStr(futureVacsSince),
                  validateAndPrepareLocalDateStr(futureVacsUntil)) mapN(SumrsQryParams.apply)

    result.toEither
  }
}