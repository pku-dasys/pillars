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

