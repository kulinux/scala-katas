name := "queens"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.0.0",
        "org.scalatest" %% "scalatest" % "3.2.0" % "test",
        "eu.timepit" %% "refined"                 % "0.9.17",
        "eu.timepit" %% "refined-cats"            % "0.9.17"
    )

// scalac options come from the sbt-tpolecat plugin so need to set any here

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
