name := "graph"

version := "1.0"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "neo4j-releases" at "http://m2.neo4j.org/content/repositories/releases/",
  "sonatype-snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
//  "com.assembla.scala-incubator" %% "graph-core" % "1.9.2",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3",
  ("eu.fakod" % "neo4j-scala_2.10" % "0.3.0")
    .exclude("org.scala-lang", "scala-library")
    .exclude("org.scala-lang", "scala-reflect")

)

connectInput in run := true

mainClass in assembly := Some("etude8.Etude8")
mainClass in Compile := Some("etude8.Etude8")
assemblyJarName in assembly := "wordChain.jar"

assemblyMergeStrategy in assembly := {
  case n if n.startsWith("META-INF/eclipse.inf") => MergeStrategy.discard
  case n if n.startsWith("META-INF/ECLIPSEF.RSA") => MergeStrategy.discard
  case n if n.startsWith("META-INF/ECLIPSE_.RSA") => MergeStrategy.discard
  case n if n.startsWith("META-INF/ECLIPSEF.SF") => MergeStrategy.discard
  case n if n.startsWith("META-INF/ECLIPSE_.SF") => MergeStrategy.discard
  case n if n.startsWith("META-INF/MANIFEST.MF") => MergeStrategy.discard
  case n if n.startsWith("META-INF/NOTICE.txt") => MergeStrategy.discard
  case n if n.startsWith("META-INF/NOTICE") => MergeStrategy.discard
  case n if n.startsWith("META-INF/LICENSE.txt") => MergeStrategy.discard
  case n if n.startsWith("META-INF/LICENSE") => MergeStrategy.discard
  case n if n.startsWith("rootdoc.txt") => MergeStrategy.discard
  case n if n.startsWith("readme.html") => MergeStrategy.discard
  case n if n.startsWith("readme.txt") => MergeStrategy.discard
  case n if n.startsWith("library.properties") => MergeStrategy.discard
  case n if n.startsWith("license.html") => MergeStrategy.discard
  case n if n.startsWith("about.html") => MergeStrategy.discard
  case _ => MergeStrategy.first

}