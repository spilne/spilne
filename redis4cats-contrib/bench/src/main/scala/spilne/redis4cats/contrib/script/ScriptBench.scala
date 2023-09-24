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
import dev.profunktor.redis4cats.{Redis, RedisCommands}
import dev.profunktor.redis4cats.effects.ScriptOutputType
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import spilne.redis4cats.contrib.script.syntax.*

class ScriptBench {

  import cats.effect.unsafe.implicits.global

  @Benchmark
  def cachedScript(state: ScriptBench.RedisState, blackhole: Blackhole): Unit = {
    val args = ScriptArgs("key1" :: Nil, "value1" :: Nil)
    blackhole.consume(state.cachedScript.eval(args).unsafeRunSync())
  }

  @Benchmark
  def nonCachedScript(state: ScriptBench.RedisState, blackhole: Blackhole): Unit = {
    val args = ScriptArgs("key1" :: Nil, "value1" :: Nil)
    blackhole.consume(state.cachedScript.origin.eval(args).unsafeRunSync())
  }
}

object ScriptBench {

  @State(Scope.Benchmark)
  class RedisState {
    import cats.effect.unsafe.implicits.global
    import dev.profunktor.redis4cats.effect.Log.NoOp._

    var client: RedisCommands[IO, String, String] = _
    var closeClient: IO[Unit] = _
    var cachedScript: CachedScript[IO, String, String, String] = _

    @Setup
    def setup(): Unit = {
      Redis[IO]
        .utf8("redis://localhost:6379")
        .allocated
        .map { case (c, cl) =>
          client = c
          closeClient = cl
        }
        .unsafeRunSync()

      cachedScript = client
        .cacheScript(
          s"""
             |redis.call("del", KEYS[1])
             |redis.call("set", KEYS[1], ARGV[1])
             |local res = redis.call("get", KEYS[1])
             |return res""".stripMargin,
          ScriptOutputType.Value
        )
        .unsafeRunSync()
    }

    @TearDown
    def clean(): Unit = {
      closeClient.unsafeRunSync()
    }
  }
}
