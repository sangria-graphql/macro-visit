name := "macro-visit"
organization := "org.sangria-graphql"
mimaPreviousArtifacts := Set("org.sangria-graphql" %% "macro-visit" % "0.1.2")

description := "Macro-based generic visitor generator"
homepage := Some(url("http://sangria-graphql.org"))
licenses := Seq("Apache License, ASL Version 2.0" → url("http://www.apache.org/licenses/LICENSE-2.0"))

scalaVersion := "2.13.1"
crossScalaVersions := Seq("2.11.12", "2.12.10", scalaVersion.value)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Xlint",
  "-Xlint:-missing-interpolator")

scalacOptions ++= {
  if (scalaVersion.value startsWith "2.11")
    Seq("-target:jvm-1.7")
  else
    Seq.empty
}

libraryDependencies ++= Seq(
  // macros
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  // testing
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.sangria-graphql" %% "sangria" % "1.0.0" % Test
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF")

// Sangria 1.0.0 (required for AST in test) has never been published for 2.13
libraryDependencies --= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) =>
      Seq("org.sangria-graphql" %% "sangria" % "1.0.0" % Test)
    case _ => Seq.empty
  }
}

excludeFilter in (Test, unmanagedSources) := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) => HiddenFileFilter || "GraphQLVisitorSpec.scala"
    case _ => NothingFilter
  }
}

// Publishing
releaseCrossBuild := true
releasePublishArtifactsAction := PgpKeys.publishSigned.value
publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := (_ ⇒ false)
publishTo := Some(
  if (version.value.trim.endsWith("SNAPSHOT"))
    "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

startYear := Some(2017)
organizationHomepage := Some(url("https://github.com/sangria-graphql"))
developers := Developer("OlegIlyenko", "Oleg Ilyenko", "", url("https://github.com/OlegIlyenko")) :: Nil
scmInfo := Some(ScmInfo(
  browseUrl = url("https://github.com/sangria-graphql-org/sangria.git"),
  connection = "scm:git:git@github.com:sangria-graphql-org/sangria.git"
))

// nice *magenta* prompt!

shellPrompt in ThisBuild := { state ⇒
  scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
}
