val isScala3 = Def.setting(
  CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 3)
)

name := "macro-visit"
organization := "org.sangria-graphql"
mimaPreviousArtifacts := {
  if (isScala3.value)
    Set.empty
  else
    Set("org.sangria-graphql" %% "macro-visit" % "0.1.2")
}

description := "Macro-based generic visitor generator"
homepage := Some(url("http://sangria-graphql.org"))
licenses := Seq(
  "Apache License, ASL Version 2.0" → url("http://www.apache.org/licenses/LICENSE-2.0"))

ThisBuild / crossScalaVersions := Seq("2.12.15", "2.13.8", "3.1.3")
ThisBuild / scalaVersion := crossScalaVersions.value.last
ThisBuild / githubWorkflowPublishTargetBranches := List()
ThisBuild / githubWorkflowBuildPreamble ++= List(
  WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting")),
  WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Check binary compatibility"))
)

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint", "-Xlint:-missing-interpolator")

scalacOptions ++= {
  if (isScala3.value)
    Seq("-Xtarget:8", "-Xcheck-macros")
  else
    Seq("-target:jvm-1.8")
}
javacOptions ++= Seq("-source", "8", "-target", "8")

libraryDependencies ++= {
  if (isScala3.value)
    Seq.empty
  else
    Seq(
      // macros
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
}

libraryDependencies ++= Seq(
  // testing
  "org.scalatest" %% "scalatest" % "3.2.12" % Test,
  ("org.sangria-graphql" %% "sangria" % "2.1.6").cross(CrossVersion.for3Use2_13) % Test
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
  )
)

// nice *magenta* prompt!
ThisBuild / shellPrompt := { state ⇒
  scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
}
