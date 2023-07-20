/*
 * Copyright 2023 spilne
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spilne.redis4cats.contrib.lock

import cats.effect.kernel.Concurrent
import dev.profunktor.redis4cats.algebra.{Scripting, Setter}
import dev.profunktor.redis4cats.effects.{ScriptOutputType, SetArg, SetArgs}

import scala.concurrent.duration.FiniteDuration

trait RedLockBase[F[_]] {
  def tryAcquire(ttl: FiniteDuration): F[Boolean]
  def tryRelease(): F[Boolean]
}

object RedLockBase {

  def simple[F[_]: Concurrent, K, V](
    redis: Scripting[F, K, V] with Setter[F, K, V]
  )(
    resource: K,
    token: V
  ): F[RedLockBase[F]] = {
    import spilne.redis4cats.contrib.script._
    import cats.syntax.all._

    val releaseLuaScript: String =
      s"""
         |if redis.call("get", KEYS[1]) == ARGV[1] then
         |    return redis.call("del", KEYS[1])
         |else
         |    return 0
         |end
      """.stripMargin

    for {
      releaseScript <- redis.cacheScript(releaseLuaScript, ScriptOutputType.Integer)
      releaseScriptArgs = ScriptArgs().withKeys(resource).withValues(token)
      lock = {
        new RedLockBase[F] {
          override def tryAcquire(ttl: FiniteDuration): F[Boolean] = {
            val setArgs = SetArgs(SetArg.Existence.Nx, SetArg.Ttl.Px(ttl))
            redis.set(resource, token, setArgs)
          }

          override def tryRelease(): F[Boolean] = {
            releaseScript
              .eval(releaseScriptArgs)
              .map(_ > 0)
          }
        }
      }
    } yield lock
  }
}
