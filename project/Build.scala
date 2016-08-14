import sbt._
import Keys._

/** Note: only the `boilerless` subproject needs to be published (`sbt + boilerless/publishLocal`).
  * TODO rm project 'core'/'tests' */
object BuildSettings {
  val paradiseVersion = "2.1.0"
  val enumeratumVersion = "1.4.9"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.github.lptk",
    version := "0.1-SNAPSHOT",
    scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps", "-deprecation"),
    scalaVersion := "2.11.8",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.10.5", "2.10.6", "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7", "2.11.8"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project("root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in core
    )
  ) aggregate(macros, core)

  lazy val macros: Project = Project("boilerless",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= (
        if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
        else Nil
      ),
      publishMavenStyle := true,
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value)
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      },
      publishArtifact in Test := false,
      pomIncludeRepository := { _ => false },
      pomExtra := {
        <url>https://github.com/LPTK/Boilerless</url>
        <licenses>
          <license>
            <name>MIT</name>
            <url>https://opensource.org/licenses/MIT</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:LPTK/Boilerless.git</url>
          <connection>scm:git:git@github.com:LPTK/Boilerless.git</connection>
        </scm>
        <developers>
          <developer>
            <id>LPTK</id>
            <name>Lionel Parreaux</name>
            <url>http://lptk.github.com</url>
          </developer>
        </developers>
      }
    )
  )

  lazy val core: Project = Project("tests",
    file("core"),
    settings = buildSettings ++ Seq(
      libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
      libraryDependencies ++= Seq("com.beachape" %% "enumeratum" % enumeratumVersion)
    )
  ) dependsOn(macros % "test->test") dependsOn(macros)
}
