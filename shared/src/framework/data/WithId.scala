package framework.data

import sttp.tapir.Mapping

/** Named tuple.
  *
  * Quite useful in Tapir endpoint definitions when you want the [[Id]] to come from path and [[Data]] to come from the
  * request body.
  *
  * For example:
  * {{{
  *   endpoint.put
  *     .in(RootPath / "document-templates" / path[DocumentTemplateId])
  *     .in(jsonBody[DocumentTemplatesUpdateRequest])
  *     .mapIn(WithId.mapping)
  *     .out(jsonBody[Boolean])
  * }}}
  */
case class WithId[+Id, +Data](
  id: Id,
  data: Data,
) {
  def tupled: (Id, Data) = (id, data)
}
object WithId {
  def tupled[Id, Data](tpl: (Id, Data)): WithId[Id, Data] =
    apply(tpl._1, tpl._2)

  /** Creates a Tapir mapping. */
  def mapping[Id, Data]: Mapping[(Id, Data), WithId[Id, Data]] =
    Mapping.from(tupled[Id, Data])(_.tupled)
}
