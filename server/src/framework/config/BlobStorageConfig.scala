package framework.config

import ciris.ConfigError
import framework.utils.seaweedfs.{SeaweedFsMasterServerUri, SeaweedFsVolumeServerPublicUri, SeaweedFsVolumeServerUri}
import cats.syntax.all.*

final case class BlobStorageConfig(
  masterServerUri: SeaweedFsMasterServerUri,
  volumeServerUri: SeaweedFsVolumeServerUri,
  volumeServerPublicUri: SeaweedFsVolumeServerPublicUri,
)
object BlobStorageConfig {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, BlobStorageConfig] = (
    ciris.env(prefix("SEAWEEDFS_MASTER_URI")).as[SeaweedFsMasterServerUri],
    ciris.env(prefix("SEAWEEDFS_VOLUME_URI")).as[SeaweedFsVolumeServerUri],
    ciris.env(prefix("SEAWEEDFS_VOLUME_PUBLIC_URI")).as[SeaweedFsVolumeServerPublicUri],
  ).parMapN(apply)
}
