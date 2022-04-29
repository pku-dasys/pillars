logLevel := Level.Warn

// find a common version number in both
// (for sbt-scalafmt) https://repo1.maven.org/maven2/org/scalameta/sbt-scalafmt_2.12_1.0/
// and (for IntelliJ) https://mvnrepository.com/artifact/org.scalameta/scalafmt-cli_2.12/
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")

// http://www.scalastyle.org/sbt.html
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
