name := "macro-visit"
organization := "org.sangria-graphql"
mimaPreviousArtifacts := Set("org.sangria-graphql" %% "macro-visit" % "0.1.2")

description := "Macro-based generic visitor generator"
homepage := Some(url("http://sangria-graphql.org"))
licenses := Seq(
  "Apache License, ASL Version 2.0" → url("http://www.apache.org/licenses/LICENSE-2.0"))

ThisBuild / crossScalaVersions := Seq("2.12.13", "2.13.5")
ThisBuild / scalaVersion := crossScalaVersions.value.last
ThisBuild / githubWorkflowPublishTargetBranches := List()
ThisBuild / githubWorkflowBuildPreamble ++= List(
  WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting")),
  WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Check binary compatibility"))
)

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint", "-Xlint:-missing-interpolator")

scalacOptions += "-target:jvm-1.8"
javacOptions ++= Seq("-source", "8", "-target", "8")

libraryDependencies ++= Seq(
  // macros
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  // testing
  "org.scalatest" %% "scalatest" % "3.2.7" % Test,
  "org.sangria-graphql" %% "sangria" % "2.1.0" % Test
)

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oF")

// Publishing
releaseCrossBuild := true
releasePublishArtifactsAction := PgpKeys.publishSigned.value
publishMavenStyle := true
Test / publishArtifact := false
pomIncludeRepository := (_ ⇒ false)
publishTo := Some(
  if (version.value.trim.endsWith("SNAPSHOT"))
    "snapshots".at("https://oss.sonatype.org/content/repositories/snapshots")
  else
    "releases".at("https://oss.sonatype.org/service/local/staging/deploy/maven2"))

startYear := Some(2017)
organizationHomepage := Some(url("https://github.com/sangria-graphql"))
developers := Developer(
  "OlegIlyenko",
  "Oleg Ilyenko",
  "",
  url("https://github.com/OlegIlyenko")) :: Nil
scmInfo := Some(
  ScmInfo(
    browseUrl = url("https://github.com/sangria-graphql/sangria.git"),
    connection = "scm:git:git@github.com:sangria-graphql/sangria.git"
  ))

// nice *magenta* prompt!
ThisBuild / shellPrompt := { state ⇒
  scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
}
