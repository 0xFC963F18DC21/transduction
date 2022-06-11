Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    name                := "transduction",
    scalaVersion        := "2.13.8",
    version             := "0.1.0",
    scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature"),
    libraryDependencies += "org.scalatest"             %% "scalatest"       % "3.2.12"   % Test
  )
  .settings(
    name := "transduction"
  )

// Tests should run sequentially, as for some reason it keeps hanging.
Test / parallelExecution := false
