organization := "net.proofpeer"

name := "ProofPeer Indent"

version := "0.3"

scalaVersion := "2.11.1"

scalacOptions += "-feature"

libraryDependencies += "net.proofpeer" %% "proofpeer-scala" % "0.2-SNAPSHOT"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
