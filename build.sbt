scalaVersion := "2.12.4"


lazy val root = project.in(file(".")).
    aggregate(escalimaJVM, escalimaJS).
    settings(
        publish := {},
        publishLocal := {}
    )

lazy val escalima = crossProject.
    crossType(CrossType.Full).
    in(file("escalima")).
    settings(
        name := "escalima",
        organization := "de.srtobi",
        version := "0.1-SNAPSHOT",
        publishArtifact in Test := false
    ).
    jvmSettings(
        libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test,
        libraryDependencies += "com.lihaoyi" %% "upickle" % "0.5.1",
        fork in Test := true,
        baseDirectory in Test := file("."),
    ).
    jsSettings(
        libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % "test",
        libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.5.1",
    )

lazy val escalimaJVM = escalima.jvm
lazy val escalimaJS = escalima.js.enablePlugins(ScalaJSBundlerPlugin)

lazy val predef = project
    .in(file("predef"))
    .dependsOn(escalimaJVM)
    .settings(
        name := "escalima-tests",
        version := "0.1-SNAPSHOT",
    )
