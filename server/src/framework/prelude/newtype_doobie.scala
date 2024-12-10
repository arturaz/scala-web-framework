package framework.prelude

import doobie.util.{Get, Put}
import doobie.postgres.implicits.*
import yantl.Newtype
import framework.utils.NamedEnum
import org.tpolecat.typename.TypeName
import doobie.util.meta.Meta

def doobieGetForNewtype[TUnderlying, TWrapperCompanion <: Newtype.WithUnderlying[TUnderlying]](
  wrapper: TWrapperCompanion
)(using
  read: Get[TUnderlying]
): Get[wrapper.Type] =
  read.map(wrapper.make(_).getOrThrow)

def doobiePutForNewtype[TUnderlying, TWrapperCompanion <: Newtype.WithUnderlying[TUnderlying]](
  wrapper: TWrapperCompanion
)(using
  write: Put[TUnderlying]
): Put[wrapper.Type] =
  write.contramap(wrapper.unwrap)

/** Constructs a [[Meta]] instance for a PostgreSQL enum. */
def doobieMetaForEnum[A: TypeName](databaseEnumName: String)(using named: NamedEnum[A]): Meta[A] =
  pgEnumStringOpt(databaseEnumName, named.fromName, named.toName)
