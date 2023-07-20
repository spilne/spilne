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

package spilne.redis4cats.contrib.script

import cats.effect.IO
import cats.effect.kernel.Resource
import com.dimafeng.testcontainers.GenericContainer
import com.dimafeng.testcontainers.munit.TestContainerForAll
import dev.profunktor.redis4cats.{Redis, RedisCommands}
import dev.profunktor.redis4cats.effect.Log
import dev.profunktor.redis4cats.effects.ScriptOutputType
import munit.{CatsEffectSuite, Location}
import org.testcontainers.containers.wait.strategy.Wait

import scala.util.Random

class CachedScriptIntegrationSuite extends CatsEffectSuite with TestContainerForAll {

  override val containerDef: GenericContainer.Def[GenericContainer] = GenericContainer.Def(
    dockerImage = "redis:7.0.5-alpine",
    exposedPorts = Seq(6379),
    waitStrategy = Wait.forListeningPort()
  )

  test("cache -> execute -> flush scripts -> execute") {
    withRedis { redis =>
      def assertScriptExists(digest: String)(implicit loc: Location): IO[Unit] =
        redis.scriptCached(digest).assertEquals(true)

      def assertScriptDoesNotExist(digest: String)(implicit loc: Location): IO[Unit] =
        redis.scriptCached(digest).assertEquals(false)

      val randomString: String = Random.nextString(10)

      for {
        _            <- redis.scriptFlush
        script       <- redis.cacheScript(s"return ARGV[1]", ScriptOutputType.Value)
        scriptDigest <- script.digest
        _            <- assertScriptExists(scriptDigest)
        _            <- script.eval(ScriptArgs().withValues(randomString)).assertEquals(randomString)
        _            <- assertScriptExists(scriptDigest)
        _            <- redis.scriptFlush
        _            <- assertScriptDoesNotExist(scriptDigest)
        _            <- script.eval(ScriptArgs().withValues(randomString)).assertEquals(randomString)
        _            <- assertScriptExists(scriptDigest)
      } yield ()
    }
  }

  private def withRedis[A](action: RedisCommands[IO, String, String] => IO[A]): IO[A] = {
    mkRedisClient.use(action)
  }

  private def mkRedisClient: Resource[IO, RedisCommands[IO, String, String]] = {
    import Log.NoOp._

    withContainers { redisContainer =>
      redisContainer.portBindings
      val redisUri = s"redis://${redisContainer.host}:${redisContainer.mappedPort(redisContainer.exposedPorts.head)}"
      Redis[IO].utf8(redisUri)
    }
  }
}
