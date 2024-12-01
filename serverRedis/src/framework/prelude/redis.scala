package framework.prelude

import dev.profunktor.redis4cats.connection.RedisURI

given Show[RedisURI] = _.underlying.toString()
