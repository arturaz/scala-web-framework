package framework.utils

import fs2.Chunk
import fs2.compression.{Compression, GunzipResult}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.util.Using
import fs2.Collector

object Gzip {

  /** Gzips an input array returning a [[Stream]]. */
  def compress[F[_]: Compression](input: IArray[Byte]): Stream[F, Byte] = {
    val gzipPipe = Compression[F].gzip()
    Stream.chunk(Chunk.array(input.unsafeArray)).through(gzipPipe)
  }

  /** Synchronously gzips an input array. */
  def compressSyncTo(input: IArray[Byte], to: Collector[Byte]): to.Out =
    compress(input)(using Compression.forSync[SyncIO]).compile.to(to).unsafeRunSync()

  /** Degzips an input array. */
  def decompress[F[_]: Compression](compressed: IArray[Byte]): Stream[F, Byte] = {
    val gunzipPipe = Compression[F].gunzip()
    Stream.chunk(Chunk.array(compressed.unsafeArray)).through(gunzipPipe).flatMap(_.content)
  }

  /** Synchronously degzips an input array. */
  def decompressSyncTo(input: IArray[Byte], to: Collector[Byte]): to.Out =
    decompress(input)(using Compression.forSync[SyncIO]).compile.to(to).unsafeRunSync()
}
