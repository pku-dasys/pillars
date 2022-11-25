val scala_version = "2.12"
val pillars_version = "3.1.1" // FIXME

interp.repositories() ::: List(
  coursierapi.MavenRepository.of("https://oss.sonatype.org/content/repositories/snapshots")
)

@

interp.configureCompiler(x => x.settings.source.value = scala.tools.nsc.settings.ScalaVersion("2.12.10"))

// Uncomment and change to use proxy
// System.setProperty("https.proxyHost", "proxy.example.com")
// System.setProperty("https.proxyPort", "3128")

import $ivy.`edu.berkeley.cs::chisel3:3.2.2`
import $ivy.`edu.berkeley.cs::chisel-iotesters:1.3.2`
import $ivy.`com.typesafe.play::play-json:2.8.0`
// import $ivy.`com.apache.logging.log4j::log4j-core:2.17.2`

// Load the pillars package (in two JAR files)
val jar_path = System.getProperty("user.dir") + s"/../target/scala-$scala_version/pillars-project_$scala_version-$pillars_version"
val jar_files = Array(jar_path + ".jar", jar_path + "-tests.jar")
for (jar <- jar_files) {
    interp.load.cp(new java.io.File(jar).toURI.toURL)
}
