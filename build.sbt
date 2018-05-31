scalaVersion := "2.12.1"

scalacOptions += "-Xfatal-warnings"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1" % Test,
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "org.typelevel" %% "cats-core" % "1.1.0"
)

scalacOptions ++= Seq("-Xfatal-warnings")

