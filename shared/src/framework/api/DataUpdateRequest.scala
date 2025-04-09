package framework.api

import framework.prelude.*
import sttp.tapir.Schema

/** A request to update some data in the server.
  *
  * Server module has extensions on [[DataUpdateRequest]] to implement the actual update.
  *
  * @param id
  *   the id of the data to update
  * @param expected
  *   the values that are expected to be in the database to prevent accidental changes
  * @param toSet
  *   the values to set
  */
case class DataUpdateRequest[Id, Data](id: Id, expected: Data, toSet: Data) derives CanEqual, Schema, CirceCodec {
  def subset[A](f: Data => A): DataUpdateRequest.DataSubset[A] =
    DataUpdateRequest.DataSubset(expected = f(expected), toSet = f(toSet))
}
object DataUpdateRequest {
  case class DataSubset[A](expected: A, toSet: A) derives CanEqual, Schema, CirceCodec {

    /** @return
      *   - `true` if the data has transitioned from `from` to `to`
      *   - `false` otherwise
      */
    def transitioned(from: A, to: A)(using CanEqual1[A]): Boolean =
      expected == from && toSet == to
  }
}
