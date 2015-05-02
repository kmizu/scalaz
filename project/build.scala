import sbt._
import Project.Setting
import Keys._

import GenTypeClass._

import java.awt.Desktop

import scala.collection.immutable.IndexedSeq

import sbtrelease._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

import com.typesafe.sbt.pgp.PgpKeys._

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi._

import sbtbuildinfo.Plugin._

import sbtunidoc.Plugin._
import sbtunidoc.Plugin.UnidocKeys._

object build extends Build {
  type Sett = Def.Setting[_]

  lazy val publishSignedArtifacts = ReleaseStep(
    action = st => {
      val extracted = st.extract
      val ref = extracted.get(thisProjectRef)
      extracted.runAggregated(publishSigned in Global in ref, st)
    },
    check = st => {
      // getPublishTo fails if no publish repository is set up.
      val ex = st.extract
      val ref = ex.get(thisProjectRef)
      Classpaths.getPublishTo(ex.get(publishTo in Global in ref))
      st
    },
    enableCrossBuild = true
  )

  def scalaCheckVersion = "1.12.2"

  private def gitHash = sys.process.Process("git rev-parse HEAD").lines_!.head

  lazy val standardSettings: Seq[Sett] = sbtrelease.ReleasePlugin.releaseSettings ++ Seq[Sett](
    organization := "org.scalaz",

    scalaVersion := "2.12.0-M1",
    crossScalaVersions := Seq("2.10.5", "2.11.6"),
    resolvers ++= (if (scalaVersion.value.endsWith("-SNAPSHOT")) List(Opts.resolver.sonatypeSnapshots) else Nil),
    resolvers += "Scala 2.12.0-M1 Modules" at "https://oss.sonatype.org/content/repositories/orgscala-lang-1202/",
    resolvers += "Scala 2.12.0-M1 Core" at "https://oss.sonatype.org/content/repositories/orgscala-lang-1201/",
    scalacOptions ++= Seq(
      // contains -language:postfixOps (because 1+ as a parameter to a higher-order function is treated as a postfix op)
      // no generic signatures, see SI-7932 and #571
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps",
      "-unchecked",
      "-Yno-generic-signatures"
    ),

    scalacOptions in (Compile, doc) <++= (baseDirectory in LocalProject("scalaz"), version) map { (bd, v) =>
      val tagOrBranch = if(v endsWith "SNAPSHOT") gitHash else ("v" + v)
      Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", "https://github.com/scalaz/scalaz/tree/" + tagOrBranch + "€{FILE_PATH}.scala")
    },

    // retronym: I was seeing intermittent heap exhaustion in scalacheck based tests, so opting for determinism.
    parallelExecution in Test := false,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1"),

    (unmanagedClasspath in Compile) += Attributed.blank(file("dummy")),

    genTypeClasses <<= (scalaSource in Compile, streams, typeClasses) map {
      (scalaSource, streams, typeClasses) =>
        typeClasses.flatMap {
          tc =>
            val typeClassSource0 = typeclassSource(tc)
            typeClassSource0.sources.map(_.createOrUpdate(scalaSource, streams.log))
        }
    },
    checkGenTypeClasses <<= genTypeClasses.map{ classes =>
      if(classes.exists(_._1 != FileStatus.NoChange))
        sys.error(classes.groupBy(_._1).filterKeys(_ != FileStatus.NoChange).mapValues(_.map(_._2)).toString)
    },
    typeClasses := Seq(),
    genToSyntax <<= typeClasses map {
      (tcs: Seq[TypeClass]) =>
      val objects = tcs.map(tc => "object %s extends To%sSyntax".format(Util.initLower(tc.name), tc.name)).mkString("\n")
      val all = "object all extends " + tcs.map(tc => "To%sSyntax".format(tc.name)).mkString(" with ")
      objects + "\n\n" + all
    },
    typeClassTree <<= typeClasses map {
      tcs => tcs.map(_.doc).mkString("\n")
    },

    showDoc in Compile <<= (doc in Compile, target in doc in Compile) map { (_, out) =>
      val index = out / "index.html"
      if (index.exists()) Desktop.getDesktop.open(out / "index.html")
    },

    credentialsSetting,
    publishSetting,
    publishArtifact in Test := false,

    // adapted from sbt-release defaults
    // (performs `publish-signed` instead of `publish`)
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishSignedArtifacts,
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),

    pomIncludeRepository := {
      x => false
    },
    pomExtra := (
      <url>http://scalaz.org</url>
        <licenses>
          <license>
            <name>BSD-style</name>
            <url>http://opensource.org/licenses/BSD-3-Clause</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:scalaz/scalaz.git</url>
          <connection>scm:git:git@github.com:scalaz/scalaz.git</connection>
        </scm>
        <developers>
          {
          Seq(
            ("runarorama", "Runar Bjarnason"),
            ("pchiusano", "Paul Chiusano"),
            ("tonymorris", "Tony Morris"),
            ("retronym", "Jason Zaugg"),
            ("ekmett", "Edward Kmett"),
            ("alexeyr", "Alexey Romanov"),
            ("copumpkin", "Daniel Peebles"),
            ("rwallace", "Richard Wallace"),
            ("nuttycom", "Kris Nuttycombe"),
            ("larsrh", "Lars Hupel")
          ).map {
            case (id, name) =>
              <developer>
                <id>{id}</id>
                <name>{name}</name>
                <url>http://github.com/{id}</url>
              </developer>
          }
        }
        </developers>
      )
  ) ++ osgiSettings ++ Seq[Sett](
    OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
  )

  def module(id: String, dir: String) =
    Project(id, file(dir)).settings(
      standardSettings : _*
    ).settings(
      scalacOptions <+= (kindProjectorJar in kindProjector).map("-Xplugin:" + _)
    ).dependsOn(
      kindProjector % "plugin->default(compile)"
    )

  lazy val scalaz = module(
    "scalaz",
    "."
  ).settings(
    unidocSettings ++ Seq[Sett](
      artifacts <<= Classpaths.artifactDefs(Seq(packageDoc in Compile)),
      packagedArtifacts <<= Classpaths.packaged(Seq(packageDoc in Compile))
    ) ++ Defaults.packageTaskSettings(packageDoc in Compile, (unidoc in Compile).map(_.flatMap(Path.allSubpaths))) : _*
  ).aggregate(
    core, concurrent, effect, example, iteratee, kindProjector
  )

  lazy val kindProjectorJar = TaskKey[String]("kindProjectorJar")

  lazy val kindProjector = {
    val kindProjectorRef = "9d9a1db322f0ab6b6f3f84ea2624c67252369665"
    val kindProjectorZip = url(s"https://github.com/non/kind-projector/archive/$kindProjectorRef.zip")

    Project(
      "kind-projector",
      file("kindProjector")
    ).settings(
      standardSettings
    ).settings(
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ % "provided"),
      kindProjectorJar <<= (packageBin in Compile).map(_.getAbsolutePath),
      (sourceGenerators in Compile) += task{
        IO.unzipURL(
          kindProjectorZip,
          (sourceManaged in Compile).value,
          new SimpleFilter(
            name => name.contains("src/main/scala/") && name.endsWith(".scala")
          )
        ).toSeq
      },
      (resourceGenerators in Compile) += task{
        val resource = (resourceManaged in Compile).value / "scalac-plugin.xml"
        val pluginXML = s"https://raw.githubusercontent.com/non/kind-projector/$kindProjectorRef/src/main/resources/scalac-plugin.xml"
        IO.download(url(pluginXML), resource)
        Seq(resource)
      }
    )
  }

  lazy val core = module(
    "core", "core"
  ).settings(
    buildInfoSettings ++ Seq[Sett](
      name := "scalaz-core",
      typeClasses := TypeClass.core,
      sourceGenerators in Compile <+= (sourceManaged in Compile) map {
        dir => Seq(GenerateTupleW(dir))
      },
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
      buildInfoPackage := "scalaz",
      osgiExport("scalaz"),
      OsgiKeys.importPackage := Seq("javax.swing;resolution:=optional", "*")
    ) : _*
  )

  lazy val concurrent = module(
    "concurrent", "concurrent"
  ).settings(
    name := "scalaz-concurrent",
    typeClasses := TypeClass.concurrent,
    osgiExport("scalaz.concurrent"),
    OsgiKeys.importPackage := Seq("javax.swing;resolution:=optional", "*")
  ).dependsOn(
    core, effect
  )

  lazy val effect = module(
    "effect", "effect"
  ).settings(
    name := "scalaz-effect",
    typeClasses := TypeClass.effect,
    osgiExport("scalaz.effect", "scalaz.std.effect", "scalaz.syntax.effect")
  ).dependsOn(
    core
  )

  lazy val iteratee = module(
    "iteratee", "iteratee"
  ).settings(
    name := "scalaz-iteratee",
    osgiExport("scalaz.iteratee")
  ).dependsOn(
    effect
  )

  lazy val example = module(
    "example", "example"
  ).settings(
    name := "scalaz-example",
    publishArtifact := false
  ).dependsOn(
    core, iteratee, concurrent
  )

  lazy val publishSetting = publishTo <<= (version).apply{
    v =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }

  lazy val credentialsSetting = credentials += {
    Seq("build.publish.user", "build.publish.password") map sys.props.get match {
      case Seq(Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
      case _                           =>
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }
  }

  lazy val genTypeClasses = TaskKey[Seq[(FileStatus, File)]]("gen-type-classes")

  lazy val typeClasses = TaskKey[Seq[TypeClass]]("type-classes")

  lazy val genToSyntax = TaskKey[String]("gen-to-syntax")

  lazy val showDoc = TaskKey[Unit]("show-doc")

  lazy val typeClassTree = TaskKey[String]("type-class-tree", "Generates scaladoc formatted tree of type classes.")

  lazy val checkGenTypeClasses = TaskKey[Unit]("check-gen-type-classes")

  def osgiExport(packs: String*) = OsgiKeys.exportPackage := packs.map(_ + ".*;version=${Bundle-Version}")
}

// vim: expandtab:ts=2:sw=2
