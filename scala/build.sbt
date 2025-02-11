val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc-2024",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-Wvalue-discard",
      "-Wnonunit-statement",
      "-Wconf:msg=(unused.*value|discarded.*value|pure.*statement):error",
      "-language:strictEquality"
    ),
    // scalacOptions += "-Ypartial-unification",
    // libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "io.getkyo" %% "kyo-prelude" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-core" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-direct" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-combinators" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-sttp" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-tapir" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-zio" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-caliban" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-cache" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-stats-otel" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-data" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-scheduler" % "0.15.1",
    libraryDependencies += "io.getkyo" %% "kyo-scheduler-zio" % "0.15.1"
  )
