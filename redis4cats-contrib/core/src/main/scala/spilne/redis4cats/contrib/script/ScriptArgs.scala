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

case class ScriptArgs[K, V](keys: List[K] = Nil, values: List[V] = Nil) {
  def withKeys(ks: List[K]): ScriptArgs[K, V] = copy(keys = ks)
  def withKeys(ks: K*): ScriptArgs[K, V] = withKeys(ks.toList)
  def withValues(vs: List[V]): ScriptArgs[K, V] = copy(values = vs)
  def withValues(vs: V*): ScriptArgs[K, V] = withValues(vs.toList)
}

object ScriptArgs {
  private val _empty: ScriptArgs[Nothing, Nothing] = ScriptArgs(Nil, Nil)

  def apply[K, V](): ScriptArgs[K, V] = empty
  def empty[K, V]: ScriptArgs[K, V] = _empty.asInstanceOf[ScriptArgs[K, V]]
}
