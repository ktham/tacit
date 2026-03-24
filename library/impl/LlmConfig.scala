package tacit.library

import caps.assumeSafe

@assumeSafe
case class LlmConfig(baseUrl: String, apiKey: String, model: String):
  override def toString: String = s"LlmConfig($baseUrl, ***, $model)" // hide API key
