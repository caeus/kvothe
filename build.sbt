name := """kvothe"""
organization := "edu.caeus"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  guice,
  "org.typelevel" %% "cats-free" % "1.4.0",
  "com.softwaremill.common" %% "tagging" % "2.2.1",
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
  "io.monix" %% "monix" % "3.0.0-RC1",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "5.0.1.201806211838-r",
  "com.dripower" %% "play-circe" % "2610.0",
  "org.gnieh" %% "diffson-play-json" % "3.1.0")++ Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.10.0")


// Adds additional packages into Twirl
//TwirlKeys.templateImports += "edu.caeus.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "edu.caeus.binders._"
