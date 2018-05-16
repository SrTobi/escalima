
sonatypeProfileName := "de.srtobi"

lazy val commonSettings = Seq(
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.6.6",
)


lazy val root = project.in(file(".")).
    aggregate(escalimaJVM, escalimaJS, downStreamTestNode, downStreamTestJVM).
    settings(
        name := "escalima",
        publishArtifact := false,
        skip in publish := true
    )

lazy val escalima = crossProject
    .crossType(CrossType.Full)
    .in(file("escalima"))
    .settings(commonSettings)
    .settings(
        name := "escalima",
        organization := "de.srtobi",
        version := "0.4",
        isSnapshot := false,
        scalacOptions in (Compile, doc) := Seq("-diagrams"),
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
        },
    )
    .jvmSettings(
        fork in Test := true,
        baseDirectory in Test := file("."),
    )

lazy val escalimaJVM = escalima.jvm
lazy val escalimaJS = escalima.js.enablePlugins(ScalaJSBundlerPlugin)

lazy val predef = project
    .in(file("predef"))
    .dependsOn(escalimaJVM)
    .settings(commonSettings)
    .settings(
        skip in publish := true
    )


lazy val downStreamTestJVM = project
    .in(file("downstream/jvm"))
    .dependsOn(escalimaJVM)
    .settings(commonSettings)
    .settings(
        fork in Test := true,
        skip in publish := true
    )

lazy val downStreamTestBrowser = project
    .in(file("downstream/browser"))
    .enablePlugins(ScalaJSPlugin)
    //.enablePlugins(ScalaJSBundlerPlugin)
    .dependsOn(escalimaJS)
    .settings(commonSettings)
    .settings(
        skip in publish := true
    )

lazy val downStreamTestNode = project
    .in(file("downstream/node"))
    .enablePlugins(ScalaJSPlugin)
    .enablePlugins(ScalaJSBundlerPlugin)
    .settings(commonSettings)
    .dependsOn(escalimaJS)
    .settings(
        skip in publish := true
    )
