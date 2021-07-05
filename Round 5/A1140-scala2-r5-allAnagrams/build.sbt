lazy val root = (project in file(".")).
  settings(
    name := "A1140-scala2-r5-allAnagrams",
    version := "1.0",
    scalaVersion := "2.13.6",
    Test / parallelExecution := false,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % Test
    ,libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.3.0"

  )
