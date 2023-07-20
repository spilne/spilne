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

package spilne.tapir.monad

import sttp.monad.MonadError

final class MonadErrorOps[F[_]](private val ME: MonadError[F]) extends AnyVal {
  def whenA[A](cond: Boolean)(fa: => F[A]): F[Unit] = if (cond) ME.map(fa)(_ => ()) else ME.unit(())
}

trait MonadErrorSyntax {
  implicit def toMonadErrorOps[F[_]](me: MonadError[F]): MonadErrorOps[F] = new MonadErrorOps(me)
}
