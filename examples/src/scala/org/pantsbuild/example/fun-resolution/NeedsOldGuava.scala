package org.pantsbuild.examples

import com.google.common.io.Files

object NeedsOldGuava {
  // This file fails to compile when downstream targets depend on a newer version of Guava
  // because the method fileTreeTraverser() is removed in 25.0-jre. However, this file
  // compiles with 24.1-jre because the method is only deprecated.
  val x = Files.fileTreeTraverser()
}
