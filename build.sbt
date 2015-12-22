// This file is distributed under the Apache 2 license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Labs.

lazy val root = (project in file(".")).
  settings(
    scalaVersion := "2.11.7",
    name := "degenerate",
    version := "0.0.0",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"
  )
