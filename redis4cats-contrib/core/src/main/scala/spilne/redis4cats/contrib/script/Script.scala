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

import dev.profunktor.redis4cats.algebra.Scripting
import dev.profunktor.redis4cats.effects.ScriptOutputType

trait Script[F[_], K, V, O] {
  def content: String
  def outputType: ScriptOutputType.Aux[V, O]
  def eval(args: ScriptArgs[K, V] = ScriptArgs.empty): F[O]
}

object Script {

  def apply[F[_], K, V, O](
    scriptCmd: Scripting[F, K, V])(script: String, out: ScriptOutputType.Aux[V, O]): Script[F, K, V, O] =
    new Script[F, K, V, O] {
      override def content: String = script
      override def outputType: ScriptOutputType.Aux[V, O] = out

      override def eval(args: ScriptArgs[K, V]): F[O] = {
        args match {
          case ScriptArgs(Nil, Nil) => scriptCmd.eval(content, outputType)
          case ScriptArgs(keys, Nil) => scriptCmd.eval(content, outputType, keys)
          case ScriptArgs(keys, values) => scriptCmd.eval(content, outputType, keys, values)
        }
      }
    }
}
