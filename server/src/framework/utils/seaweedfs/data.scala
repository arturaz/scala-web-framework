package framework.utils.seaweedfs

import framework.utils.{NewtypeString, NewtypeUri}
import org.http4s.Uri

/** The URI for the SeaweedFs master server.
  *
  * @see
  *   - https://github.com/seaweedfs/seaweedfs/wiki/Components#master-service
  *   - https://github.com/seaweedfs/seaweedfs/wiki/Master-Server-API
  */
object SeaweedFsMasterServerUri extends NewtypeUri
type SeaweedFsMasterServerUri = SeaweedFsMasterServerUri.Type

/** The URI for the SeaweedFs volume server. This will usually be not publically accessible.
  *
  * @see
  *   - https://github.com/seaweedfs/seaweedfs/wiki/Components#volume-service
  *   - https://github.com/seaweedfs/seaweedfs/wiki/Volume-Server-API
  */
object SeaweedFsVolumeServerUri extends NewtypeUri
type SeaweedFsVolumeServerUri = SeaweedFsVolumeServerUri.Type

/** The publicly accessible URI for the SeaweedFs volume server for retrieving the data.
  *
  * @see
  *   - https://github.com/seaweedfs/seaweedfs/wiki/Components#volume-service
  *   - https://github.com/seaweedfs/seaweedfs/wiki/Volume-Server-API
  */
object SeaweedFsVolumeServerPublicUri extends NewtypeUri {
  given Conversion[SeaweedFsVolumeServerPublicUri, SeaweedFsVolumeServerUri] = v =>
    SeaweedFsVolumeServerUri.make(unwrap(v)).getOrThrow
}
type SeaweedFsVolumeServerPublicUri = SeaweedFsVolumeServerPublicUri.Type

/** An identifier for a SeaweedFs file. */
object SeaweedFsFileId extends NewtypeString {
  extension (fid: Type) {

    /** Constructs the URI of the SeaweedFs file. */
    def at(uri: SeaweedFsVolumeServerUri): Uri = uri / unwrap(fid)
  }
}
type SeaweedFsFileId = SeaweedFsFileId.Type
