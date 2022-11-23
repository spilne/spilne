package dobirne.redis4cats.contrib.lock

import cats.effect.kernel.Concurrent
import dev.profunktor.redis4cats.algebra.{Scripting, Setter}
import dev.profunktor.redis4cats.effects.{ScriptOutputType, SetArg, SetArgs}

import java.util.UUID
import scala.concurrent.duration.FiniteDuration

trait RedLockBase[F[_]] {
  def tryAcquire(ttl: FiniteDuration): F[Boolean]
  def tryRelease(): F[Boolean]
}

object RedLockBase {

  def simple[F[_]: Concurrent](
    redis: Scripting[F, String, String] with Setter[F, String, String]
  )(
    resource: String,
    token: String = UUID.randomUUID().toString
  ): F[RedLockBase[F]] = {
    import dobirne.redis4cats.contrib.script._
    import cats.syntax.all._

    val releaseLuaScript =
      s"""
         |if redis.call("get", KEYS[1]) == ARGV[1] then
         |    return redis.call("del", KEYS[1])
         |else
         |    return 0
         |end
      """.stripMargin

    for {
      releaseScript <- redis.cacheScript(releaseLuaScript, ScriptOutputType.Integer)
      releaseScriptArgs = ScriptArgs(resource :: Nil, token :: Nil)
      lock = {
        new RedLockBase[F] {
          override def tryAcquire(ttl: FiniteDuration): F[Boolean] = {
            val setArgs = SetArgs(SetArg.Existence.Nx, SetArg.Ttl.Px(ttl))
            redis.set(resource, token, setArgs)
          }

          override def tryRelease(): F[Boolean] = {
            releaseScript.eval(releaseScriptArgs).map(_ > 0)
          }
        }
      }
    } yield lock
  }
}
