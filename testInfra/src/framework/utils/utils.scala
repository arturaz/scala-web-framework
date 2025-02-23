package framework.utils

import java.nio.file.Path

/** Traverses down from the given directory to find the root of the git repository. */
def findGitRepositoryRoot(from: Path = Path.of(".")): Path = {
  var current = from.toAbsolutePath

  while (!current.resolve(".git").toFile.exists()) {
    current = current.getParent
    if (current == null) {
      throw new Exception(s"Could not find git repository root starting from ${from.toAbsolutePath}")
    }
  }

  current
}
