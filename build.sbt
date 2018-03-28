val projectName = "guardrail"
name := projectName
organization in ThisBuild := "com.twilio"
version in ThisBuild := "0.33.0-SNAPSHOT"

crossScalaVersions := Seq("2.11.11", "2.12.3")
scalaVersion in ThisBuild := crossScalaVersions.value.last

val akkaVersion = "10.0.10"
val catsVersion = "0.9.0"
val circeVersion = "0.8.0"
val http4sVersion = "0.17.6"
val scalatestVersion = "3.0.1"

mainClass in assembly := Some("com.twilio.swagger.codegen.CLI")

lazy val runExample = taskKey[Unit]("Run with example args")
// TODO: akka.NotUsed should exist in all generated sources, but there are no import verifying tests yet.
fullRunTask(runExample, Test, "com.twilio.swagger.codegen.CLI", """
  --defaults --import akka.NotUsed
  --client --specPath modules/sample/src/main/resources/petstore.json --outputPath modules/sample/src/main/scala --packageName clients.http4s --framework http4s
  --client --specPath modules/sample/src/main/resources/petstore.json --outputPath modules/sample/src/main/scala --packageName clients.akkaHttp --framework akka-http
  --server --specPath modules/sample/src/main/resources/petstore.json --outputPath modules/sample/src/main/scala --packageName servers
  --client --specPath modules/sample/src/main/resources/plain.json --outputPath modules/sample/src/main/scala --packageName tests.dtos
  --server --specPath modules/sample/src/main/resources/raw-response.yaml --outputPath modules/sample/src/main/scala --packageName raw.server
  --server --specPath modules/sample/src/test/resources/server1.yaml --outputPath modules/sample/src/main/scala --packageName tracer.servers --tracing
  --client --specPath modules/sample/src/test/resources/server1.yaml --outputPath modules/sample/src/main/scala --packageName tracer.clients --tracing
  --server --specPath modules/sample/src/test/resources/server2.yaml --outputPath modules/sample/src/main/scala --packageName tracer.servers --tracing
  --client --specPath modules/sample/src/test/resources/server2.yaml --outputPath modules/sample/src/main/scala --packageName tracer.clients --tracing
  --client --specPath modules/sample/src/main/resources/alias.yaml --outputPath modules/sample/src/main/scala --packageName alias.client
  --server --specPath modules/sample/src/main/resources/alias.yaml --outputPath modules/sample/src/main/scala --packageName alias.server
  --client --specPath modules/sample/src/main/resources/edgecases/defaults.yaml --outputPath modules/sample/src/main/scala --packageName edgecases.defaults
""".replaceAllLiterally("\n", " ").split(' ').filter(_.nonEmpty): _*)

artifact in (Compile, assembly) := {
  val art = (artifact in (Compile, assembly)).value
  art.copy(`classifier` = Some("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

addCommandAlias("cli", "runMain com.twilio.swagger.codegen.CLI")

val resetSample = TaskKey[Unit]("resetSample", "Reset sample module")

resetSample := { "git clean -fdx modules/sample/src modules/sample/target" ! }

addCommandAlias("example", "; resetSample ; runExample ; sample/test")
addCommandAlias("testSuite", "; test ; resetSample; runExample ; sample/test")

publishMavenStyle := true

val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalatestVersion % Test
)

val codegenSettings = Seq(
  resolvers ++= Seq(
    Resolver.url("scalameta", url("http://dl.bintray.com/scalameta/maven"))(Resolver.ivyStylePatterns)
  ),
  libraryDependencies ++= testDependencies ++ Seq(
    "org.scalameta" %% "scalameta" % "2.0.1"
    , "io.swagger" % "swagger-parser" % "1.0.32"
    , "org.tpolecat" %% "atto-core"  % "0.6.0"
    , "org.typelevel" %% "cats" % catsVersion
  )
  // Dev
  , scalacOptions in ThisBuild ++= Seq(
      "-Ypartial-unification",
      "-language:higherKinds",
      "-Xexperimental",
      "-Ydelambdafy:method",
      "-Xlint:-unused",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-target:jvm-1.8",
      "-encoding", "utf8"
    )
  , parallelExecution in Test := true
  , fork := true
  , offline := true
)

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= testDependencies
  )
  .dependsOn(codegen)

lazy val codegen = (project in file("modules/codegen"))
  .settings(
    (name := s"${projectName}-core") +:
    codegenSettings
  )

lazy val sample = (project in file("modules/sample"))
  .settings(
    codegenSettings
    , initialCommands in console := """
      |import cats._
      |import cats.data.EitherT
      |import cats.implicits._
      |import fs2.Strategy
      |import fs2.Task
      |import fs2.interop.cats._
      |import io.circe._
      |import java.time._
      |import org.http4s._
      |import org.http4s.client._
      |import org.http4s.client.blaze._
      |import org.http4s.dsl._
      |import org.http4s.multipart._
      |import scala.concurrent.Await
      |import scala.concurrent.ExecutionContext.Implicits.global
      |import scala.concurrent.duration._
      |import scala.meta._
      |""".stripMargin
    , libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-http" % akkaVersion
      , "com.typesafe.akka" %% "akka-http-testkit" % akkaVersion
      , "io.circe" %% "circe-core" % circeVersion
      , "io.circe" %% "circe-generic" % circeVersion
      , "io.circe" %% "circe-java8" % circeVersion
      , "io.circe" %% "circe-parser" % circeVersion
      , "org.http4s" %% "http4s-blaze-client" % http4sVersion
      , "org.http4s" %% "http4s-blaze-server" % http4sVersion
      , "org.http4s" %% "http4s-circe" % http4sVersion
      , "org.http4s" %% "http4s-dsl" % http4sVersion
      , "org.scalatest" %% "scalatest" % scalatestVersion % Test
      , "org.typelevel" %% "cats" % catsVersion
    )
  )

watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.scala").get

logBuffered in Test := false
