organization := "net.proofpeer"

name := "ProofPeer Indent"

version := "0.5-SNAPSHOT"

scalaVersion := "2.11.2"

lazy val root = project.in(file(".")).aggregate(rootJS, rootJVM)

lazy val rootJS = project.in(file("proofpeer-indent-js")).settings(scalaJSSettings: _*).settings(
  name := "proofpeer-indent", 
  organization := "net.proofpeer", 
  scalaVersion := "2.11.2",
  unmanagedSourceDirectories in Compile +=
    (baseDirectory.value / "..") / "proofpeer-indent-shared" / "src" / "main" / "scala",
  libraryDependencies += "net.proofpeer" %%% "proofpeer-general" % "0.1-SNAPSHOT"
)

lazy val rootJVM = project.in(file("proofpeer-indent-jvm")).settings(
  name := "proofpeer-indent",
  organization := "net.proofpeer",
  scalaVersion := "2.11.2",
  unmanagedSourceDirectories in Compile +=
    (baseDirectory.value / "..") / "proofpeer-indent-shared" / "src" / "main" / "scala",
  libraryDependencies += "net.proofpeer" %% "proofpeer-general" % "0.1-SNAPSHOT",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)
