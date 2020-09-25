scala_library(
  name = "needs_old_guava",
  sources = [
      "NeedsOldGuava.scala",
  ],
  dependencies = [
      "3rdparty/jvm/com/google/guava",
  ],
)

scala_library(
  sources = [
      "FunResolution.scala",
  ],
  dependencies = [
      ":needs_old_guava",
      "3rdparty/jvm/org/eclipse/lsp4j",
  ],
)

