package de.thm.mni.hybridcomputing.util

import scala.collection.mutable
import scala.ref.WeakReference

class DynamicCache[Key <: AnyRef, Value <: AnyRef](compute: Key => Value) {
    
    private class Keyy(val k: WeakReference[Key]) {
        override def equals(obj: Any): Boolean = obj match
            case obj: Keyy => (for (
                a <- k.get;
                b <- obj.k.get
            ) yield a eq b).getOrElse(false)
            case _ => false
    }
    
    private val _cache = mutable.Map[Keyy, Value]()
    
    private def sanitize() = {
        this._cache.filterInPlace({ case (k, v) => k.k.get.isDefined })
    }
    
    def apply(key: Key): Value = {
        this.sanitize()
        
        this._cache.getOrElseUpdate(Keyy(WeakReference(key)), compute(key))
    }
}