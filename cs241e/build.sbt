scalaVersion := "2.13.8"

libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.0.8" % "test"

libraryDependencies += "com.lihaoyi" % "pprint_2.13" % "0.7.0"

Compile / scalaSource := baseDirectory.value / "src"

Test / scalaSource := baseDirectory.value / "test" / "src"

Compile / packageSrc / artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    "for-marmoset.zip"
}

Compile / packageSrc / mappings := {
          (Compile / sources).value pair Path.rebase(baseDirectory.value / "src", "src/")
}

Compile / packageSrc / mappings ++= {
          ((baseDirectory.value / "test") ** "*") pair Path.rebase(baseDirectory.value / "test", "test/")
}
