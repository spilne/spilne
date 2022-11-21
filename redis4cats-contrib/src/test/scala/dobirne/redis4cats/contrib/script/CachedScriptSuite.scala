package dobirne.redis4cats.contrib.script

import cats.effect.IO
import com.dimafeng.testcontainers.GenericContainer
import com.dimafeng.testcontainers.munit.TestContainerForAll
import dev.profunktor.redis4cats.Redis
import dev.profunktor.redis4cats.effect.Log
import dev.profunktor.redis4cats.effects.ScriptOutputType
import munit.CatsEffectSuite
import org.testcontainers.containers.wait.strategy.Wait

class CachedScriptSuite extends CatsEffectSuite with TestContainerForAll {
  implicit val log: Log[IO] = Log.NoOp.instance

  private val redisPort = 6379

  override val containerDef: GenericContainer.Def[GenericContainer] = GenericContainer.Def(
    dockerImage = "redis:5.0.8-alpine",
    exposedPorts = Seq(redisPort),
    waitStrategy = Wait.forListeningPort()
  )

  test("basic test") {
    withContainers { redisContainer =>
      val redisUri = s"redis://${redisContainer.containerIpAddress}:${redisContainer.mappedPort(redisPort)}"

      Redis[IO]
        .utf8(redisUri)
        .evalMap(_.cacheScript("return 1", ScriptOutputType.Integer))
        .use(_.evalSha)
        .assertEquals(1L)
    }
  }
}
