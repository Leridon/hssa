package de.thm.mni.hybridcomputing.util

object MultiMap {
    type MultiMap[Key, Value] = Map[Key, Seq[Value]]
    
    extension [Key, Value](map: MultiMap[Key, Value]) { 
        def entries(): Seq[(Key, Seq[Value])] = map.toSeq

        def valueSet(): Set[Value] = map.values.flatMap(identity).toSet
            
        def getAll(key: Key): Seq[Value] = map.get(key).map(_.toSeq).getOrElse(Seq())
            
        def getFirst(key: Key, filter: Value => Boolean = (_: Value) => true): Option[Value] = map.get(key).map(_.toSeq).getOrElse(Seq()).find(filter)
        
        def getUnique(key: Key): Value = map(key).head
    }
    
    def MultiMap[Key, Value](init: (Key, Value)*): MultiMap[Key, Value] = {
        init.groupBy(_._1).map(kv => kv._1 -> kv._2.map(_._2))
    }
}