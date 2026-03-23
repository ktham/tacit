package tacit.library

import language.experimental.captureChecking

import java.io.{BufferedReader, InputStreamReader}
import java.util.concurrent.TimeUnit

object ProcessOps:
  /** Drains an input stream into a StringBuilder on the current thread. */
  private def drainStream(stream: java.io.InputStream): StringBuilder =
    val reader = BufferedReader(InputStreamReader(stream))
    try
      val sb = StringBuilder()
      var line = reader.readLine()
      while line != null do
        if sb.nonEmpty then sb.append('\n')
        sb.append(line)
        line = reader.readLine()
      sb
    finally reader.close()

  def exec(
    command: String,
    args: List[String] = List.empty,
    workingDir: Option[String] = None,
    timeoutMs: Long = 30000
  )(using pp: ProcessPermission): ProcessResult =
    CommandValidator.validate(command, pp)
    val cmdList = new java.util.ArrayList[String]()
    cmdList.add(command)
    args.foreach(cmdList.add)
    val pb = new ProcessBuilder(cmdList)
    workingDir.foreach(d => pb.directory(java.io.File(d)))
    val process = pb.start().nn
    try
      // Drain stdout and stderr on separate threads to avoid deadlock
      // when the process output fills the OS pipe buffer.
      @volatile var stdout = ""
      @volatile var stderr = ""
      val t1 = Thread(() => stdout = drainStream(process.getInputStream).toString)
      val t2 = Thread(() => stderr = drainStream(process.getErrorStream).toString)
      t1.setDaemon(true)
      t2.setDaemon(true)
      t1.start()
      t2.start()
      val finished = process.waitFor(timeoutMs, TimeUnit.MILLISECONDS)
      if !finished then
        process.destroyForcibly()
        t1.join(1000)
        t2.join(1000)
        throw RuntimeException(s"Process '$command' timed out after ${timeoutMs}ms")
      t1.join()
      t2.join()
      ProcessResult(process.exitValue(), stdout, stderr)
    catch
      case e: Exception =>
        process.destroyForcibly()
        throw e

  def execOutput(
    command: String,
    args: List[String] = List.empty
  )(using pp: ProcessPermission): String =
    exec(command, args).stdout
