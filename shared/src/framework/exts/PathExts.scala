package framework.exts

import java.nio.file.Path

implicit class PathExts(private val path: Path) extends AnyVal {
  infix def /(segment: String): Path = path.resolve(segment)
}
