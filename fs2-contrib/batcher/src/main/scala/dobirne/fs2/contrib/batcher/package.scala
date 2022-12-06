package dobirne.fs2.contrib

package object batcher {
  type Batcher[F[_], T] = fs2.Pipe[F, T, fs2.Chunk[T]]
  type IdGenerator[F[_], Id, In] = In => F[Id]
}
