package framework.exts

import sttp.model.Uri
import sttp.model.Uri.{QuerySegment, QuerySegmentEncoding}

import scala.annotation.targetName

extension (uri: Uri) {
  infix def /(p: String, ps: String*): Uri = uri.addPath(p, ps*)

  def addQuerySegment(q: String, encoding: Uri.Encoding = QuerySegmentEncoding.StandardValue): Uri =
    uri.addQuerySegment(QuerySegment.Plain(q, encoding))

  def addQuerySegmentKV(
    k: String,
    v: String,
    keyEncoding: Uri.Encoding = QuerySegmentEncoding.Standard,
    valueEncoding: Uri.Encoding = QuerySegmentEncoding.Standard,
  ): Uri =
    uri.addQuerySegment(QuerySegment.KeyValue(k, v))
}
