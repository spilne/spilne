/*
 * Copyright (c) 2022 the purebits contributors.
 * See the project homepage at: https://pure-bits.github.io/docs
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

package dobirne.fs2.contrib.batcher

abstract case class Request[F[_], In, Out] private[batcher] (data: In) {

  private[batcher] def complete(out: Either[Throwable, Out]): F[Unit]
}

object Request {
  type Void[F[_], In] = Request[F, In, Unit]
}