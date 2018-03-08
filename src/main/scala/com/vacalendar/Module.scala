package com.vacalendar

import cats.effect.Effect
import cats.implicits._

import org.http4s.HttpService

import doobie.hikari.HikariTransactor
import org.flywaydb.core.Flyway

import com.vacalendar.conf.DatabaseConfig

import com.vacalendar.validation.{ QryParamsValidationInterpreter, ServiceValidationInterpreter }
import com.vacalendar.repository.{ EmployeeRepoInterpreter, 
                                   EmployeeSummaryRepoInterpreter, 
                                   VacationRepoInterpreter, 
                                   PositionRepoInterpreter }
import com.vacalendar.service.{ EmployeeService, EmployeeSummaryService, VacationService }
import com.vacalendar.endpoints.{ EmployeeEndpoints, 
                                  EmployeeSummaryEndpoints, 
                                  EmployeeVacationsEndpoints,
                                  EndpointErrorHandler }

class Module[F[_]](implicit F: Effect[F]) {

  def dbTransactor(dbConfig: DatabaseConfig): F[HikariTransactor[F]] =
    HikariTransactor.newHikariTransactor[F](dbConfig.driver, dbConfig.url, dbConfig.user, dbConfig.password)

  def migrateDb(xa: HikariTransactor[F]): F[Unit] = 
    xa.configure { ds =>
      F.delay {
        val fw = new Flyway()
        fw.setDataSource(ds)
        fw.migrate()
        ()
      }
    }

  def dropDb(xa: HikariTransactor[F]): F[Unit] =
    xa.configure { ds =>
      F.delay {
        val fw = new Flyway()
        fw.setDataSource(ds)
        fw.clean()
        ()
      }
    }

  def httpServices(xa: HikariTransactor[F]): HttpService[F] = {

    val emplRepo = new EmployeeRepoInterpreter[F](xa)
    val vacRepo = new VacationRepoInterpreter[F](xa)
    val posRepo = new PositionRepoInterpreter[F](xa)
    val emplSummaryRepo = new EmployeeSummaryRepoInterpreter[F](xa)
    
    val emplService = new EmployeeService[F](emplRepo, posRepo, ServiceValidationInterpreter)
    val vacService = new VacationService[F](vacRepo, emplRepo, ServiceValidationInterpreter)
    val emplSummaryService = new EmployeeSummaryService[F](emplSummaryRepo)

    val errHandler = new EndpointErrorHandler[F]()

    val emplEndpoints = new EmployeeEndpoints[F](emplService, QryParamsValidationInterpreter, errHandler).service
    val emplSummaryEndpoints = new EmployeeSummaryEndpoints[F](emplSummaryService, QryParamsValidationInterpreter, errHandler).service
    val vacEndpoints = new EmployeeVacationsEndpoints[F](vacService, QryParamsValidationInterpreter, errHandler).service

    (emplEndpoints <+>
    emplSummaryEndpoints <+>
    vacEndpoints)
  }
}