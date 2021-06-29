lazy val root = (project in file(".")).
  settings(
    name := "A1140-scala2-r4-bst",
    version := "1.0",
    scalaVersion := "2.13.6",
    Test / parallelExecution := false,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % Test
    , resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases"
    , libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.21"
    , testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
    , logBuffered := false
    , Test / parallelExecution := false
    , Test / fork := true
    , Test / outputStrategy := Some(StdoutOutput)
    ,libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"

  )
