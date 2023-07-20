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

import cats.Functor
import cats.syntax.all._
import cats.effect.kernel.Concurrent
import dev.profunktor.redis4cats.algebra.Scripting
import dev.profunktor.redis4cats.effects.ScriptOutputType

final class ScriptCommandsOps[F[_], K, V](private val cmd: Scripting[F, K, V]) extends AnyVal {

  def script[O](content: String, out: ScriptOutputType.Aux[V, O]): Script[F, K, V, O] =
    Script(cmd)(content, out)

  def cacheScript[O](script: Script[F, K, V, O])(implicit F: Concurrent[F]): F[CachedScript[F, K, V, O]] =
    CachedScript(cmd)(script)

  def cacheScript[O](
    content: String,
    out: ScriptOutputType.Aux[V, O]
  )(implicit
    F: Concurrent[F]
  ): F[CachedScript[F, K, V, O]] =
    cacheScript(script(content, out))

  def scriptCached(digest: String)(implicit F: Functor[F]): F[Boolean] =
    cmd.scriptExists(digest).map(_.headOption.getOrElse(false))
}

trait ScriptCommandsSyntax {

  final implicit def toScriptCommandsOps[F[_], K, V](cmd: Scripting[F, K, V]): ScriptCommandsOps[F, K, V] =
    new ScriptCommandsOps(cmd)
}
