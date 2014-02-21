organization := "net.proofpeer"

name := "ProofPeer Indent"

version := "0.2-SNAPSHOT"

scalaVersion := "2.10.3"

scalacOptions += "-feature"

libraryDependencies += "net.proofpeer" %% "proofpeer-scala" % "0.1-SNAPSHOT"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
