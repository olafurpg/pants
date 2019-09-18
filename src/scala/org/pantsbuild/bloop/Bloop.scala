package org.pantsbuild.bloop

import _root_.bloop.config.ConfigEncoderDecoders._
import _root_.bloop.config.{Config => C}
import coursier._
import java.nio.file.Paths
import io.circe.syntax._
import scala.util.control.NoStackTrace
import scala.sys.process._
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.nio.file.PathMatcher
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.util.stream.Collectors
import scala.collection.JavaConverters._
import scala.collection.mutable
import java.util.concurrent.TimeUnit
import ujson.Value
import java.nio.file.attribute.FileTime
import ujson.Str

object Bloop {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("missing <workspace>")
    } else if (args.sameElements(Array("--help"))) {
      println("bloop-install <sourcedir> <target>")
    } else if (args.length < 2) {
      println("missing <target>")
    } else {
      val buildroot = Paths.get(args(0))
      val target = args(1)
      val isCached = args.contains("--with-cache")
      val timer = new Timer()
      bloopInstall(buildroot, target, isCached) match {
        case None =>
          println("bloopInstall failed")
          sys.exit(1)
        case Some(result) =>
          println(
            s"Exported ${result.exportedTargetCount} project(s) in $timer"
          )
      }
    }
  }

  case class BloopInstallResult(exportedTargetCount: Int)

  def bloopInstall(
      buildroot: Path,
      target: String,
      isCached: Boolean
  ): Option[BloopInstallResult] = {
    val pantsd = buildroot.resolve(".pants.d")
    if (!Files.isDirectory(pantsd)) {
      Files.createDirectories(pantsd)
    }

    val query = target.replaceAll("[^a-zA-Z0-9]", "")

    val export = pantsd.resolve(s"$query.json")
    val bloopRoot = buildroot.resolve(".bloop")
    Files.createDirectories(bloopRoot)

    if (!isCached || !Files.isRegularFile(export)) {
      val command = List[String](
        buildroot.resolve("pants").toString(),
        "--export-libraries-sources",
        s"--export-output-file=$export",
        "export",
        target
      )
      Process(command, Some(buildroot.toFile)).!
    }

    if (Files.isRegularFile(export)) {
      val text =
        new String(Files.readAllBytes(export), StandardCharsets.UTF_8)
      val json = ujson.read(text)
      val cycles = Cycles.findConnectedComponents(json)

      val scalaReflect = coursier
        .Fetch()
        .addDependencies(
          // NOTE(olafur) scala-compiler seems to be needed :/
          dep"org.scala-lang:scala-compiler:2.12.10"
        )
        .run()
        .map(_.toPath())
      val scalaJars = coursier
        .Fetch()
        .addDependencies(
          dep"org.scala-lang:scala-compiler:2.12.10",
          dep"jline:jline:2.14.6"
        )
        .run()
      def mkGlob(str: String): PathMatcher = {
        FileSystems
          .getDefault()
          .getPathMatcher(
            "glob:" + buildroot
              .resolve(str.replaceAllLiterally("**/*", "**"))
              .toString()
          )
      }
      val targets = json.obj("targets").obj
      val libraries = json.obj("libraries").obj
      val bloopProjects: Seq[C.Project] = targets.iterator.map {
        case (id, target) =>
          val isLogging = id == "util/util-logging/src/main/scala/com/twitter/logging:logging"
          val baseDirectories = for {
            roots <- target.obj.get("roots").toList
            root <- roots.arr
            sourceRoot <- root.obj.get("source_root")
          } yield Paths.get(sourceRoot.str)
          val baseDirectory = baseDirectories.headOption.getOrElse(buildroot)
          val jsonPath = bloopRoot.resolve(id + ".json")
          val out = bloopRoot.resolve(makeFilename(id))
          val classDirectory = out.resolve("classes")
          Files.createDirectories(classDirectory)
          val (globs, excludes) = target.obj.get("globs") match {
            case None =>
              baseDirectories.map { dir =>
                val path = buildroot.resolve(dir).resolve("**")
                Str(path.toString())
              } -> Nil
            case Some(globs) =>
              val ex = globs.obj.get("exclude") match {
                case None => Nil
                case Some(excludes) =>
                  for {
                    exclude <- excludes.arr.toSeq
                    globs <- exclude.obj.get("globs").toSeq
                    glob <- globs.arr
                  } yield mkGlob(glob.str)
              }
              globs.obj("globs").arr -> ex
          }
          val sources: List[Path] = globs.iterator.flatMap { glob =>
            // NOTE(olafurpg): it's not ideal that we reimplement the glob
            // expansion logic here. It would be nice to use an official
            // library instead.
            if (glob.str.contains("*")) {
              val parent = buildroot.resolve(glob.str).getParent()
              val includes = mkGlob(glob.str)
              val stream: java.util.stream.Stream[Path] =
                if (parent.endsWith("**")) {
                  if (Files.isDirectory(parent.getParent()))
                    Files.walk(parent.getParent())
                  else java.util.stream.Stream.empty()
                } else if (Files.isDirectory(parent)) {
                  Files.list(parent)
                } else {
                  java.util.stream.Stream.empty()
                }

              stream
                .map[Path] { path =>
                  if (path.isAbsolute()) path.toAbsolutePath()
                  else buildroot.resolve(path).toAbsolutePath()
                }
                .filter { path =>
                  Files.isRegularFile(path)
                }
                .filter { path =>
                  includes.matches(path) &&
                  !excludes.exists(_.matches(path))
                }
                .collect(Collectors.toList())
                .asScala
                .toList
            } else {
              val path = Paths.get(glob.str)
              val abspath =
                if (path.isAbsolute()) path
                else buildroot.resolve(path)
              List(abspath)
            }
          }.toList
          val dependsOn = (for {
            dependency <- target.obj("targets").arr.iterator
            acyclicDependency = cycles.parents.getOrElse(
              dependency.str,
              dependency.str
            )
            if acyclicDependency != id
            if targets(acyclicDependency)
              .obj("pants_target_type")
              .str != "files"
          } yield acyclicDependency).toList
          val targetDependencies: List[Path] = (for {
            dependency <- target.obj("targets").arr.iterator
          } yield bloopRoot
            .resolve(makeFilename(dependency.str))
            .resolve("classes")).toList
          val libraryDependencies: List[Value] = (for {
            dependency <- target.obj("libraries").arr.iterator
            library <- libraries.get(dependency.str)
          } yield library).toList
          def getLibraryDependencies(key: String): List[Path] =
            (for {
              lib <- libraryDependencies
              default <- lib.obj.get(key)
            } yield Paths.get(default.str))
          val libraryDependencySources: List[C.Module] =
            for {
              source <- getLibraryDependencies("sources")
            } yield C.Module(
              "",
              "",
              "",
              None,
              artifacts = List(
                C.Artifact(
                  "",
                  classifier = Some("sources"),
                  None,
                  path = source
                )
              )
            )
          val libraryDependencyClasspaths: List[Path] =
            getLibraryDependencies("default") ++
              getLibraryDependencies("shaded") ++
              getLibraryDependencies("linux-x86_64") ++
              getLibraryDependencies("thrift9")
          val knownConfigs =
            Set("default", "linux-x86_64", "thrift9", "sources", "shaded")
          val unknownConfigs = libraryDependencies.flatMap(
            lib => lib.obj.keys.toSet -- knownConfigs
          )
          if (unknownConfigs.nonEmpty) {
            println(
              s"[warning] Unknown configs: ${unknownConfigs.mkString(",")}"
            )
          }
          val javaHome =
            Option(System.getProperty("java.home")).map(Paths.get(_))
          C.Project(
            id,
            directory = baseDirectory,
            sources,
            dependencies = dependsOn.toList,
            targetDependencies ++ libraryDependencyClasspaths,
            out,
            classDirectory,
            scala = Some(
              C.Scala(
                "org.scala-lang",
                "scala-compiler",
                "2.12.10",
                List.empty[String],
                scalaJars.iterator.map(_.toPath).toList,
                None,
                setup = Some(
                  C.CompileSetup(
                    C.Mixed,
                    addLibraryToBootClasspath = true,
                    addCompilerToClasspath = false,
                    addExtraJarsToClasspath = false,
                    manageBootClasspath = true,
                    filterLibraryFromClasspath = true
                  )
                )
              )
            ),
            java = Some(C.Java(Nil)),
            sbt = None,
            test = None,
            platform = Some(C.Platform.Jvm(C.JvmConfig(javaHome, Nil), None)),
            resolution = Some(C.Resolution(libraryDependencySources)),
            resources = None
          )
      }.toSeq
      val byName = bloopProjects.map(p => p.name -> p).toMap
      val transitiveClasspath = mutable.Map.empty[String, List[Path]]
      val isVisited = mutable.Set.empty[String]
      var stack = List.empty[String]
      val detectedCycles = mutable.Set.empty[String]
      def getTransitiveClasspath(name: String): List[Path] = {
        if (isVisited(name)) {
          transitiveClasspath.getOrElse(name, {
            // pprint.log(name :: stack)
            detectedCycles += name
            Nil
          })
        } else {
          isVisited += name
          val result = transitiveClasspath.getOrElseUpdate(
            name, {
              val buf = mutable.Set.empty[Path]
              buf ++= byName(name).classpath
              // buf ++= scalaReflect
              byName(name).dependencies.foreach { dep =>
                stack ::= name
                buf ++= getTransitiveClasspath(dep)
                stack = stack.tail
              }
              val children = cycles.children.getOrElse(name, Nil)
              children.foreach { child =>
                buf ++= getTransitiveClasspath(child)
              }
              buf.toList.sorted
            }
          )
          result
        }
      }
      // pprint.log(detectedCycles)
      val fullClasspathProjects = bloopProjects.map { p =>
        val children = cycles.children.getOrElse(p.name, Nil)
        val extraSources = children.flatMap(child => byName(child).sources)
        val extraDependencies = children.iterator
          .flatMap(child => byName(child).dependencies)
          .filter(_ != p.name)
          .toSeq
        p.copy(
          classpath = getTransitiveClasspath(p.name),
          sources = (p.sources ++ extraSources).distinct,
          dependencies = (p.dependencies ++ extraDependencies).distinct
        )
      }
      fullClasspathProjects.foreach { bloop =>
        if (cycles.parents.contains(bloop.name)) {} else {
          val out = bloopRoot.resolve(makeFilename(bloop.name) + ".json")
          val json = C.File(BloopVersion, bloop).asJson.spaces2
          Files.write(out, json.getBytes(StandardCharsets.UTF_8))
        }
      }
      Some(BloopInstallResult(fullClasspathProjects.size))
    } else {
      None
    }
  }

  class Timer() {
    private val start = System.nanoTime()
    override def toString(): String = {
      val elapsed = System.nanoTime() - start
      val ms = TimeUnit.NANOSECONDS.toMillis(elapsed)
      s"${ms}ms"
    }
  }

  val BloopVersion = "1.3.2"
  val nonAlphanumeric = "[^a-zA-Z0-9]".r

  def makeFilename(target: String): String = {
    nonAlphanumeric.replaceAllIn(target, "")
  }

}
