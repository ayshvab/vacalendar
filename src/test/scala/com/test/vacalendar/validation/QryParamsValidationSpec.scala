package com.test.vacalendar.validation

import java.time.LocalDate
import org.scalatest._
import cats.data._

import com.vacalendar.endpoints.QryParams._
import com.vacalendar.validation.{ QryParamsValidationInterpreter => V }
import com.vacalendar.errors._

class QueryParamsSpec extends WordSpec
                            with Matchers {

  "Employees query params" when {
    "valid raw query params" should {
      "return prepared query params 1" in {

        val rawOrderBy = Some("employeeId")
        val rawFirstName = Some("John")
        val rawLastName = Some("Luther")
        val rawPosId = Some(1L)

        val result = V.validateAndPrepareEmplsQryParams(oBy = rawOrderBy,
                                                        fn = rawFirstName,
                                                        ln = rawLastName,
                                                        rawPosId)

        val expected = EmplsQryParams(orderByParams = Some(OrderByParams(field = "employee_id", asc = true)),
                                      firstName =  Some("John"),
                                      lastName =  Some("Luther"),
                                      positionId = Some(1))
        result shouldEqual Right(expected)
      }

      "return prepared query params 2" in {

        val rawOrderBy = Some("-created")

        val result = V.validateAndPrepareEmplsQryParams(oBy = rawOrderBy,
                                                        fn = None,
                                                        ln = None,
                                                        posId = None)

        val expected = EmplsQryParams(orderByParams = Some(OrderByParams(field = "created", asc = false)))
        result shouldEqual Right(expected)
      }
    }

    "not valid raw query params" should {
      "return errors 1" in {

        val rawOrderBy = Some("employee_id")
        val rawFirstName = Some("John")
        val rawLastName = Some("Luther")
        val rawPosId = Some(1L)

        val result = V.validateAndPrepareEmplsQryParams(oBy = rawOrderBy,
                                                        fn = rawFirstName,
                                                        ln = rawLastName,
                                                        rawPosId)

        val expected = NonEmptyList.one(NotValidQueryParam("employee_id"))
        result shouldEqual Left(expected)
      }

      "return errors 2" in {

        val rawOrderBy = Some("--positionId")
        val rawFirstName = Some("John")
        val rawLastName = Some("Luther")
        val rawPosId = Some(1L)

        val result = V.validateAndPrepareEmplsQryParams(oBy = rawOrderBy,
                                                        fn = rawFirstName,
                                                        ln = rawLastName,
                                                        rawPosId)

        val expected = NonEmptyList.one(NotValidQueryParam("--positionId"))
        result shouldEqual Left(expected)
      }
    }
  }

  "Vacation query params" when {

    "valid raw query params" should {
      "return prepared query params 1" in {

        val rawOrderBy = Some("-vacationId")
        val rawSince = Some("2018-03-03.._")
        val rawUntil = Some("2018-08-13")

        val result = V.validateAndPrepareVacsQryParams(rawOrderBy = rawOrderBy,
                                                       since = rawSince,
                                                       until = rawUntil)

        val expected = VacsQryParams(orderByParams = Some(OrderByParams(field = "vacation_id", asc = false)),
                                     since =  Some(DateParams(after = Some(LocalDate.of(2018, 3, 3)))),
                                     until =  Some(DateParams(exact = Some(LocalDate.of(2018, 8, 13)))))

        result shouldEqual Right(expected)
      }

      "return prepared query params 2" in {

        val rawOrderBy = Some("vacationId")
        val rawSince = Some("2018-03-03..2018-03-10")
        val rawUntil = Some("_..2018-08-13")

        val result = V.validateAndPrepareVacsQryParams(rawOrderBy = rawOrderBy,
                                                       since = rawSince,
                                                       until = rawUntil)

        val expected = VacsQryParams(orderByParams = Some(OrderByParams(field = "vacation_id", asc = true)),
                                     since =  Some(DateParams(after = Some(LocalDate.of(2018, 3, 3)), before = Some(LocalDate.of(2018, 3, 10)))),
                                     until =  Some(DateParams(before = Some(LocalDate.of(2018, 8, 13)))))

        result shouldEqual Right(expected)
      }
    }

    "not valid raw query params" should {
      "return errors" in {

        val rawOrderBy = Some("-vacation_Id")
        val rawSince = Some("2018-03-03..")
        val rawUntil = Some("2018-20-13")

        val result = V.validateAndPrepareVacsQryParams(rawOrderBy = rawOrderBy,
                                                       since = rawSince,
                                                       until = rawUntil)

        val expected = NonEmptyList(NotValidQueryParam("-vacation_Id"), 
                                    List(NotValidLocalDateStringParam("2018-03-03.."),
                                         NotValidLocalDateStringParam("2018-20-13")))

        result shouldEqual Left(expected)
      }
    }
  }

  "Employee summaries query params" when {

    "valid raw query params" should {
      "return prepared query params" in {

        val result = V.validateAndPrepareEmplSummariesQryParams(oBy = Some("remainedVacationDays"),
                                                                emplId = None,
                                                                fn = None,
                                                                ln = None,
                                                                posId = Some(1L),
                                                                posTitle = Some("developer"),
                                                                isOnVac = Some("t"),
                                                                pastVacsSince = Some("2018-03-03..2018-03-10"),
                                                                pastVacsUntil = None,
                                                                currVacSince = None,
                                                                currVacUntil = Some("_..2018-03-10"),
                                                                futureVacsSince = Some("2018-04-03..2018-08-10"),
                                                                futureVacsUntil = Some("2018-04-03..2018-08-10"))

        val expected = EmplSummariesQryParams(orderByParams = Some(OrderByParams(field = "remainedVacationDays", asc = true)),
                                              positionId =  Some(1L),
                                              positionTitle = Some("developer"),
                                              isOnVac = Some(true),
                                              pastVacsSince = Some(DateParams(after = Some(LocalDate.of(2018, 3, 3)), before = Some(LocalDate.of(2018, 3, 10)))),
                                              currVacUntil = Some(DateParams(before = Some(LocalDate.of(2018, 3, 10)))),
                                              futureVacsSince = Some(DateParams(after = Some(LocalDate.of(2018, 4, 3)), before = Some(LocalDate.of(2018, 8, 10)))),
                                              futureVacsUntil = Some(DateParams(after = Some(LocalDate.of(2018, 4, 3)), before = Some(LocalDate.of(2018, 8, 10)))))                                     

        result shouldEqual Right(expected)
      }
    }
    
    "not valid raw query params" should {
      "return errors" in {

        val result = V.validateAndPrepareEmplSummariesQryParams(oBy = Some("vacationId"),
                                                                emplId = None,
                                                                fn = None,
                                                                ln = None,
                                                                posId = Some(1L),
                                                                posTitle = Some("developer"),
                                                                isOnVac = Some("t"),
                                                                pastVacsSince = Some("2018-03-03.."),
                                                                pastVacsUntil = None,
                                                                currVacSince = None,
                                                                currVacUntil = Some("_..2018-03-"),
                                                                futureVacsSince = Some("2018-04-03..2018-08-10"),
                                                                futureVacsUntil = Some("2018-04-03..2018-08-10"))

        val expected = NonEmptyList(NotValidQueryParam("vacationId"), 
                                    List(NotValidLocalDateStringParam("2018-03-03.."),
                                          NotValidLocalDateStringParam("_..2018-03-")))                                  

        result shouldEqual Left(expected)
      }
    }

  }
  
}