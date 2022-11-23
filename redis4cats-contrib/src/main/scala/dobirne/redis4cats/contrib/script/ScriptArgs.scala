package dobirne.redis4cats.contrib.script

case class ScriptArgs[K, V](keys: List[K] = Nil, values: List[V] = Nil) {
  def withKeys(ks: List[K]): ScriptArgs[K, V] = copy(keys = ks)
  def withValues(vs: List[V]): ScriptArgs[K, V] = copy(values = vs)
}

object ScriptArgs {
  private val _empty: ScriptArgs[Nothing, Nothing] = ScriptArgs(Nil, Nil)

  def apply[K, V](): ScriptArgs[K, V] = empty
  def empty[K, V]: ScriptArgs[K, V] = _empty.asInstanceOf[ScriptArgs[K, V]]
  def keys[K, V](ks: List[K]): ScriptArgs[K, V] = empty.withKeys(ks)
  def values[K, V](vs: List[V]): ScriptArgs[K, V] = empty.withValues(vs)
}
