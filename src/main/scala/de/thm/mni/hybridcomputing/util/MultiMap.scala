package de.thm.mni.hybridcomputing.util

class MultiMap[Key, Value](initialdata: (Key, Value)*) {
    private val data = initialdata.groupBy(_._1).map(kv => kv._1 -> kv._2.map(_._2))
    
    def contains(key: Key): Boolean = this.data.contains(key)
    
    def keys(): Set[Key] = data.keySet
    
    def entries(): Seq[(Key, Seq[Value])] = data.toSeq
    
    def getAll(key: Key): Seq[Value] = this.data.get(key).map(_.toSeq).getOrElse(Seq())
    
    def get(key: Key, filter: Value => Boolean = _ => true): Option[Value] = this.data.get(key).map(_.toSeq).getOrElse(Seq()).find(filter)
}