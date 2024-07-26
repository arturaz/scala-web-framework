package framework.utils.seaweedfs

import _root_.io.circe.Json
import cats.FlatMap
import cats.effect.kernel.Async
import cats.syntax.all.*
import fs2.Pipe
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.dsl.*
import org.http4s.multipart.{Multiparts, Part}
import sttp.model.Uri.QuerySegment
import org.http4s.headers.ETag

/** @param fid
  *   the base file id, use [[fileIds]] instead of accessing this key.
  * @see
  *   https://github.com/seaweedfs/seaweedfs/wiki/Master-Server-API#assign-a-file-key
  */
case class AssignResponse(
  fid: SeaweedFsFileId,
  url: String,
  publicUrl: String,
  count: Int,
) derives CirceDecoder {
  def fileIds: NonEmptySet[SeaweedFsFileId] =
    count match {
      case 0 => throw new IllegalStateException("SeaweedFs returned no files")
      case 1 => NonEmptySet.of(fid)
      case _ =>
        NonEmptySet.fromSetUnsafe(
          (1 to count).iterator.map(fileNo => SeaweedFsFileId(show"${fid}_$fileNo")).toSortedSet
        )
    }
}

/** @see
  *   https://github.com/seaweedfs/seaweedfs/wiki/Master-Server-API#assign-a-file-key
  */
case class AssignSingleResponse(
  fid: SeaweedFsFileId,
  url: String,
  publicUrl: String,
) derives CirceDecoder

/** @see https://github.com/seaweedfs/seaweedfs/wiki/Volume-Server-API#upload-file */
case class UploadResponse(
  eTag: ETag
)

trait SeaweedFsApi[F[_]] {

  /** @param count
    *   the number of files to assign. Must be at least 1.
    * @see
    *   https://github.com/seaweedfs/seaweedfs/wiki/Master-Server-API#assign-a-file-key
    */
  def assign(count: Int = 1)(using SeaweedFsMasterServerUri): F[AssignResponse]

  /** [[assign]] where the count is 1. */
  def assignSingle(using SeaweedFsMasterServerUri): F[AssignSingleResponse]

  /** Uploads a file to SeaweedFs. */
  def upload(volumeServer: SeaweedFsVolumeServerUri, fid: SeaweedFsFileId, bytes: Stream[F, Byte]): F[UploadResponse]

  /** Deletes a file from SeaweedFs.
    *
    * @see
    *   https://github.com/seaweedfs/seaweedfs/wiki/Volume-Server-API#delete-file
    */
  def delete(volumeServer: SeaweedFsVolumeServerUri, fid: SeaweedFsFileId): F[Unit]
}
object SeaweedFsApi {
  def apply[F[_]: Async](client: Client[F]): SeaweedFsApi[F] = new SeaweedFsApi[F] {
    def assignUri(using master: SeaweedFsMasterServerUri): sttp.model.Uri = master / "dir" / "assign"

    override def assign(count: Int)(using SeaweedFsMasterServerUri): F[AssignResponse] = {
      client.expect[AssignResponse](assignUri.addQuerySegment(QuerySegment.KeyValue("count", show"$count")))(using
        jsonOf[F, AssignResponse]
      )
    }

    override def assignSingle(using master: SeaweedFsMasterServerUri): F[AssignSingleResponse] = {
      client.expect(assignUri)(using jsonOf[F, AssignSingleResponse])
    }

    override def upload(
      volumeServer: SeaweedFsVolumeServerUri,
      fid: SeaweedFsFileId,
      bytes: Stream[F, Byte],
    ): F[UploadResponse] = {
      for {
        multiparts <- Multiparts.forSync[F]
        multipart <- multiparts.multipart(Vector(Part.fileData("file", fid.show, bytes)))
        request = Request[F](Method.POST, fid.at(volumeServer), headers = multipart.headers).withEntity(multipart)
        response <- client.run(request).use { response =>
          Async[F].delay {
            val eTag =
              response.headers
                .get[ETag]
                .getOrElse(throw new Exception(s"ETag header not found in upload file $fid to $volumeServer response"))

            UploadResponse(eTag)
          }
        }
      } yield response
    }

    override def delete(volumeServer: SeaweedFsVolumeServerUri, fid: SeaweedFsFileId): F[Unit] =
      client.expect[Unit](Request[F](Method.DELETE, fid.at(volumeServer)))
  }
}
