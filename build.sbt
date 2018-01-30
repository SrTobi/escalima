name := "escalima"

version := "0.1"

scalaVersion := "2.12.4"

lazy val root = project.in(file(".")).
    aggregate(predef).
    settings(
        publish := {},
        publishLocal := {}
    )

lazy val escalima = crossProject.
    crossType(CrossType.Full).
    in(file("escalima")).
    settings(
        name := "escalima",
        version := "0.1-SNAPSHOT",
        libraryDependencies += "com.lihaoyi" %% "upickle" % "0.5.1",
    ).
    jvmSettings(
        // Add JVM-specific settings here
        //libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0"
    ).
    jsSettings(
        // Add JS-specific settings here
    )

lazy val escalimaJVM = escalima.jvm
lazy val escalimaJS = escalima.js

lazy val predef = project
    .in(file("predef"))
    .dependsOn(escalimaJVM)
    .settings(
        name := "escalima-tests",
        version := "0.1-SNAPSHOT",
        libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test
    )
