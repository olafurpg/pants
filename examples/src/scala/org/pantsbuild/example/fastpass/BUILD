# Copyright 2014 Pants project contributors (see CONTRIBUTORS.md).
# Licensed under the Apache License, Version 2.0 (see LICENSE).

scala_library(
  name = "a",
  sources = ["A.scala"],
)


scala_library(
  name = "b",
  sources = ["B.scala"],
)

scala_library(
  name = "c",
  sources = ["C.scala"],
  dependencies = [":a"],
)

java_library(
  name = "c2",
  sources = ["C2.java"],
  dependencies = [":a"],
)

java_library(
  name = "c3",
  scope = "runtime",
  sources = ["C3.java"],
  dependencies = [":a"],
)

java_library(
  name = "c4",
  scope = "compile test",
  sources = ["C4.java"],
  dependencies = [":a"],
)

scala_library(
  name = "d",
  sources = ["D.scala"],
  dependencies = [":b", ":c"],
  strict_deps = True,
)

scala_library(
  name = "d2",
  sources = ["D2.scala"],
  dependencies = [":a"],
  exports = [":a"],
)

scala_library(
  name = "e",
  sources = ["E.scala"],
  dependencies = [":b", ":c"],
  strict_deps = False,
)


junit_tests(
  name = "f",
  sources = ["F.scala"],
  dependencies = [":e"],
  extra_jvm_options=["-disableassertions"],
  strict_deps = False,
)

scala_library(
  name = "g_scala",
  sources = ["G1.scala"],
  java_sources = [":g_java"]
)

java_library(
  name = "g_java",
  sources = ["G2.java"],
  dependencies = [":g_scala"]
)

jvm_binary(
  name = "g_bin",
  dependencies = [":g_java"],
  main = "org.pantsbuild.example.fastpass.G2",
)
