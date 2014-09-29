organization := "net.proofpeer"

name := "ProofPeer Indent"

version := "0.4-SNAPSHOT"

scalaVersion := "2.11.2"

scalacOptions += "-feature"

libraryDependencies += "net.proofpeer" %% "proofpeer-general" % "0.1-SNAPSHOT"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
