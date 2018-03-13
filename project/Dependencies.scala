import sbt._

object Dependencies {

  object Versions {
    val Cats                     = "1.0.1"
    val CatsEffect               = "0.8"
    val Fs2                      = "0.10.2"
    val Http4s                   = "0.18.2"
    val Tsec                     = "0.0.1-M9"
    val Circe                    = "0.9.1"
    val Doobie                   = "0.5.0"
    val Flyway                   = "5.0.5"
    val ScalaTest                = "3.0.3"
    val ScalaCheck               = "1.13.4"
    val Logback                  = "1.2.1"
    val TestcontainersScala      = "0.15.0"
    val TestcontainersPostgresql = "1.6.0"
    val PureConfig               = "0.9.0"
    val Fastparse                = "1.0.0"
  }

  object Libraries {
    lazy val fastparse                = "com.lihaoyi"           %% "fastparse"              % Versions.Fastparse
    lazy val cats                     = "org.typelevel"         %% "cats-core"              % Versions.Cats
    lazy val catsEffect               = "org.typelevel"         %% "cats-effect"            % Versions.CatsEffect
    lazy val fs2Core                  = "co.fs2"                %% "fs2-core"               % Versions.Fs2
    lazy val fs2IO                    = "co.fs2"                %% "fs2-io"                 % Versions.Fs2
    lazy val http4sServer             = "org.http4s"            %% "http4s-blaze-server"    % Versions.Http4s
    lazy val http4sDsl                = "org.http4s"            %% "http4s-dsl"             % Versions.Http4s
    lazy val http4sCirce              = "org.http4s"            %% "http4s-circe"           % Versions.Http4s
    lazy val tsecJwtMac               = "io.github.jmcardon"    %% "tsec-jwt-mac"           % Versions.Tsec
    lazy val circeCore                = "io.circe"              %% "circe-core"             % Versions.Circe
    lazy val circeGeneric             = "io.circe"              %% "circe-generic"          % Versions.Circe
    lazy val flyway                   = "org.flywaydb"          %  "flyway-core"            % Versions.Flyway
    lazy val doobieCore               = "org.tpolecat"          %% "doobie-core"            % Versions.Doobie
    lazy val doobiePostgres           = "org.tpolecat"          %% "doobie-postgres"        % Versions.Doobie
    lazy val doobieTest               = "org.tpolecat"          %% "doobie-scalatest"       % Versions.Doobie
    lazy val logback                  = "ch.qos.logback"        %  "logback-classic"        % Versions.Logback
    lazy val pureConfig               = "com.github.pureconfig" %% "pureconfig"             % Versions.PureConfig
    lazy val doobieHikari             = "org.tpolecat"          %% "doobie-hikari"          % Versions.Doobie
    lazy val scalaTest                = "org.scalatest"         %% "scalatest"              % Versions.ScalaTest                % Test
    lazy val scalaCheck               = "org.scalacheck"        %% "scalacheck"             % Versions.ScalaCheck               % Test
    lazy val testcontainersScala      = "com.dimafeng"          %% "testcontainers-scala"   % Versions.TestcontainersScala      % Test
    lazy val testcontainersPostgresql = "org.testcontainers"    % "postgresql"              % Versions.TestcontainersPostgresql % Test
  }
}
