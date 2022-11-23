package dobirne.redis4cats.contrib.script

import scala.collection.mutable.ListBuffer

case class ScriptArgs[+K, +V](keys: List[K] = Nil, values: List[V] = Nil)

object ScriptArgs {
  private val _empty: ScriptArgs[Nothing, Nothing] = ScriptArgs(Nil, Nil)

  def apply[K, V](): ScriptArgs[K, V] = empty
  def empty[K, V]: ScriptArgs[K, V] = _empty.asInstanceOf[ScriptArgs[K, V]]
  def keys[K, V](ks: List[K]): ScriptArgs[K, V] = _empty.copy(keys = ks)
  def values[K, V](vs: List[V]): ScriptArgs[K, V] = _empty.copy(values = vs)

  def builder[K, V]: Builder[K, V] = new Builder(ListBuffer.empty, ListBuffer.empty)

  class Builder[K, V](private val keys: ListBuffer[K], private val values: ListBuffer[V]) {
    def key(k: K): Builder[K, V] = { keys.addOne(k); this }
    def value(v: V): Builder[K, V] = { values.addOne(v); this }
    def result: ScriptArgs[K, V] = ScriptArgs(keys.toList, values.toList)
  }
}
