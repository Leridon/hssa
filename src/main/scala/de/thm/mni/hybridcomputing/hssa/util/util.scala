package de.thm.mni.hybridcomputing.hssa.util

def TODO(s: String = ""): Nothing = {
    throw new NotImplementedError(s)
}

class IDEqual[T <: AnyRef](val value: T) {
    override def equals(obj: Any): Boolean = obj match {
        case that: IDEqual[_] => this.value eq that.value
        case _ => false
    }
}


extension [A](self: List[A]) {
    def filterIsInstance[B](implicit c: Class[B]): List[B] = self.filter(c.isInstance).map(c.cast)
    def foldMap[B, C](start: B)(f: (B, A) => (B, C)): (B, List[C]) = {
        val res = self.foldLeft[(B, List[C])]((start, Nil))({
            case ((running, acc), element) => {
                val (b, c) = f(running, element)
                (b, c :: acc)
            }
        })
        
        (res._1, res._2.reverse)
    }
}

extension [A](self: Set[A]) {
    def filterIsInstance[B](implicit c: Class[B]): Set[B] = self.filter(c.isInstance).map(c.cast)
}

def isSet[A](collection: Seq[A]): Boolean = collection.toSet.size == collection.size

def counts[A, K](collection: Seq[A], key: A => K): Map[K, Int] = collection.groupBy(key).view.mapValues(_.size).toMap.withDefaultValue(0)
def counts[A](collection: Seq[A]): Map[A, Int] = counts[A, A](collection, identity)

def mapBy[A, K](collection: Seq[A])(f: A => K): Map[K, A] = collection.groupBy(f).view.mapValues(_.head).toMap
