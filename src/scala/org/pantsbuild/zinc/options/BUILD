# Copyright 2017 Pants project contributors (see CONTRIBUTORS.md).
# Licensed under the Apache License, Version 2.0 (see LICENSE).

scala_library(
  provides=scala_artifact(
    org='org.pantsbuild',
    name='zinc-options',
    repo=public,
    publication_metadata=pants_library('The SBT incremental compiler for nailgun')
  ),
  strict_deps=True,
  dependencies=[
      '3rdparty/jvm/org/scala-lang:scala-compiler',
  ],
  platform='java8',
)
