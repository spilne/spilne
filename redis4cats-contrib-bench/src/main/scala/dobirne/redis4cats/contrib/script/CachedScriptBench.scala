package dobirne.redis4cats.contrib.script

import cats.effect.IO
import dev.profunktor.redis4cats.{Redis, RedisCommands}
import dev.profunktor.redis4cats.effects.ScriptOutputType
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class CachedScriptBench {

  import cats.effect.unsafe.implicits.global

  @Benchmark
  def cachedScript(state: CachedScriptBench.RedisState, blackhole: Blackhole): Unit = {
    val args = ScriptArgs("key1" :: Nil, "value1" :: Nil)
    blackhole.consume(state.cachedScript.eval(args).unsafeRunSync())
  }

  @Benchmark
  def nonCachedScript(state: CachedScriptBench.RedisState, blackhole: Blackhole): Unit = {
    val args = ScriptArgs("key1" :: Nil, "value1" :: Nil)
    blackhole.consume(state.cachedScript.origin.eval(args).unsafeRunSync())
  }
}

object CachedScriptBench {

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
        .map {
          case (c, cl) =>
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
