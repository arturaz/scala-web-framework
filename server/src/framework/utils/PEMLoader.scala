package framework.utils

import org.bouncycastle.openssl.jcajce.JcaPEMKeyConverter
import org.bouncycastle.openssl.{PEMKeyPair, PEMParser}

import java.io.StringReader
import java.security.KeyPair
import scala.util.Try

object PEMLoader {

  /** Loads a PEM key pair.
    *
    * @see
    *   https://github.com/web-push-libs/webpush-java/wiki/VAPID#read-the-pem-directly
    */
  def load(pem: String): Try[KeyPair] = Try {
    val parser = PEMParser(StringReader(pem))
    val keyPair = parser.readObject().asInstanceOf[PEMKeyPair]
    JcaPEMKeyConverter().getKeyPair(keyPair)
  }
}
