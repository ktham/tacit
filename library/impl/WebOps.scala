package tacit.library

import language.experimental.captureChecking

import java.net.{URI, HttpURLConnection}
import java.nio.charset.StandardCharsets

object WebOps:
  private val TimeoutMs = 10000

  /** Extracts and validates the host from a URL, guarding against null from malformed URIs. */
  private def validatedHost(url: String)(using net: Network): Unit =
    val uri = URI(url)
    val host = uri.getHost
    if host == null then throw SecurityException(s"Invalid URL (no host): $url")
    net.validateHost(host)

  /** Reads the response body, falling back to the error stream on HTTP error codes. */
  private def readResponse(conn: HttpURLConnection): String =
    val code = conn.getResponseCode
    val stream = if code >= 400 then conn.getErrorStream else conn.getInputStream
    if stream == null then return s"HTTP $code (no response body)"
    try String(stream.readAllBytes(), StandardCharsets.UTF_8)
    finally stream.close()

  def httpGet(url: String)(using net: Network): String =
    validatedHost(url)
    val conn = URI(url).toURL.openConnection().asInstanceOf[HttpURLConnection]
    try
      conn.setRequestMethod("GET")
      conn.setConnectTimeout(TimeoutMs)
      conn.setReadTimeout(TimeoutMs)
      readResponse(conn)
    finally conn.disconnect()

  def httpPost(url: String, body: String, contentType: String)(using net: Network): String =
    validatedHost(url)
    val conn = URI(url).toURL.openConnection().asInstanceOf[HttpURLConnection]
    try
      conn.setRequestMethod("POST")
      conn.setDoOutput(true)
      conn.setRequestProperty("Content-Type", contentType)
      conn.setConnectTimeout(TimeoutMs)
      conn.setReadTimeout(TimeoutMs)
      val os = conn.getOutputStream
      try os.write(body.getBytes(StandardCharsets.UTF_8))
      finally os.close()
      readResponse(conn)
    finally conn.disconnect()
