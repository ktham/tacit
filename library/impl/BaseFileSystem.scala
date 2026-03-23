package tacit.library

import language.experimental.captureChecking

import java.nio.file.Path

/** Shared path-validation and classified-path logic for file-system implementations. */
abstract class BaseFileSystem extends FileSystem:

  protected def normalizedRoot: Path
  protected def normalizedClassified: Set[Path]
  protected def pathCheck(relativePath: String): Boolean

  protected final def checkPath(resolved: Path): Unit =
    val rel = normalizedRoot.relativize(resolved).toString
    if rel.nonEmpty && !pathCheck(rel) then
      throw SecurityException(s"Access denied: path '$rel' did not pass the check")

  protected final def isClassifiedPath(p: Path): Boolean =
    normalizedClassified.exists(cp => p.startsWith(cp))

  protected final def requireNotClassified(p: Path, op: String): Unit =
    if isClassifiedPath(p) then
      throw SecurityException(
        s"Access denied: '$op' is not allowed on classified path $p. Use classified operations instead."
      )

  protected final def requireClassified(p: Path, op: String): Unit =
    if !isClassifiedPath(p) then
      throw SecurityException(
        s"Access denied: '$op' is only allowed on classified paths, but $p is not classified."
      )
