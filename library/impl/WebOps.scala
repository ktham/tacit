package tacit.library

import java.net.{URI, HttpURLConnection}
import language.experimental.captureChecking

object WebOps:
  private val TimeoutMs = 10000

  /** Extracts and validates the host from a URL, guarding against null from malformed URIs. */
  private def validatedHost(url: String)(using net: Network): Unit =
    val uri = URI(url)
    val host = uri.getHost
    if host == null then throw SecurityException(s"Invalid URL (no host): $url")
    net.validateHost(host)

  def httpGet(url: String)(using net: Network): String =
    validatedHost(url)
    val conn = URI(url).toURL.openConnection().asInstanceOf[HttpURLConnection]
    try
      conn.setRequestMethod("GET")
      conn.setConnectTimeout(TimeoutMs)
      conn.setReadTimeout(TimeoutMs)
      val is = conn.getInputStream
      try String(is.readAllBytes())
      finally is.close()
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
      try os.write(body.getBytes)
      finally os.close()
      val is = conn.getInputStream
      try String(is.readAllBytes())
      finally is.close()
    finally conn.disconnect()
