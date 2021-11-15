name := "macro-visit"
organization := "org.sangria-graphql"
mimaPreviousArtifacts := Set("org.sangria-graphql" %% "macro-visit" % "0.1.2")

description := "Macro-based generic visitor generator"
homepage := Some(url("http://sangria-graphql.org"))
licenses := Seq(
  "Apache License, ASL Version 2.0" → url("http://www.apache.org/licenses/LICENSE-2.0"))

ThisBuild / crossScalaVersions := Seq("2.12.15", "2.13.7")
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
  "org.scalatest" %% "scalatest" % "3.2.10" % Test,
  "org.sangria-graphql" %% "sangria" % "2.1.6" % Test
)

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oF")

// Release
ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.StartsWith(Ref.Tag("v")))
ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

startYear := Some(2017)
organizationHomepage := Some(url("https://github.com/sangria-graphql"))
developers := Developer(
  "OlegIlyenko",
  "Oleg Ilyenko",
  "",
  url("https://github.com/OlegIlyenko")) :: Nil
scmInfo := Some(
  ScmInfo(
    browseUrl = url("https://github.com/sangria-graphql/macro-visit"),
    connection = "scm:git:git@github.com:sangria-graphql/macro-visit.git"
  ))

// nice *magenta* prompt!
ThisBuild / shellPrompt := { state ⇒
  scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
}
