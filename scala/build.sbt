val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc-2024",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-Wvalue-discard",
      "-Wnonunit-statement",
      "-Wconf:msg=(unused.*value|discarded.*value|pure.*statement):error"
    ),
    // scalacOptions += "-Ypartial-unification",
    // libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0",
    libraryDependencies += "org.typelevel" %% "kittens" % "3.5.0",
    libraryDependencies += "org.typelevel" %% "cats-mtl" % "1.4.0",
    libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.9",
    libraryDependencies += "org.typelevel" %% "mouse" % "1.3.2"
  )
