sonatypeProfileName := "de.srtobi"


lazy val root = project.in(file(".")).
    aggregate(escalimaJVM, escalimaJS).
    settings(
        publishArtifact := false,
        skip in publish := true
    )

lazy val escalima = crossProject.
    crossType(CrossType.Full).
    in(file("escalima")).
    settings(
        name := "escalima",
        organization := "de.srtobi",
        version := "0.1",
        isSnapshot := false,
        publishArtifact in Test := false,
        useGpg := true,
        licenses := Seq("MIT license" -> url("http://www.opensource.org/licenses/mit-license.php")),
        homepage := Some(url("https://srtobi.github.io/escalima/")),
        scmInfo := Some(
            ScmInfo(
                url("https://github.com/SrTobi/escalima"),
                "scm:git@github.com:SrTobi/escalima.git"
            )
        ),
        developers := List(
            Developer(
                id = "SrTobi",
                name = "Tobias Kahlert",
                email = "code.databyte@gmail.com",
                url = url("https://srtobi.de")
            )
        ),
        publishMavenStyle := true,
        publishTo := {
            val nexus = "https://oss.sonatype.org/"
            if (isSnapshot.value)
                Some("snapshots" at nexus + "content/repositories/snapshots")
            else
                Some("releases"  at nexus + "service/local/staging/deploy/maven2")
        }
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
    .settings()
