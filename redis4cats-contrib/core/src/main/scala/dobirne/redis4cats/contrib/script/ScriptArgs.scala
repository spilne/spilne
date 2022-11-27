package dobirne.redis4cats.contrib.script

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
