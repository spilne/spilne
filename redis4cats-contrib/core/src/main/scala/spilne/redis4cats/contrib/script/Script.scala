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
