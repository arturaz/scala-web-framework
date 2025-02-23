package framework.exts

import java.nio.file.Path

extension (path: Path) {
  infix def /(segment: String): Path = path.resolve(segment)
}
