package framework.prelude

import doobie.util.{Read, Write}
import doobie.postgres.implicits.*
import neotype.Newtype
import framework.utils.NamedEnum
import org.tpolecat.typename.TypeName
import doobie.util.meta.Meta

def doobieReadForNewtype[TUnderlying, TWrapperCompanion <: Newtype[TUnderlying]](wrapper: TWrapperCompanion)(using
  read: Read[TUnderlying]
): Read[wrapper.Type] =
  read.map(wrapper.make(_).getOrThrow)

def doobieWriteForNewtype[TUnderlying, TWrapperCompanion <: Newtype[TUnderlying]](wrapper: TWrapperCompanion)(using
  write: Write[TUnderlying]
): Write[wrapper.Type] =
  write.contramap(wrapper.unwrap)

/** Constructs a [[Meta]] instance for a PostgreSQL enum. */
def doobieMetaForEnum[A: TypeName](databaseEnumName: String)(using named: NamedEnum[A]): Meta[A] =
  pgEnumStringOpt(databaseEnumName, named.fromName, named.toName)
