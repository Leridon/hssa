package de.thm.mni.hybridcomputing.hssa.util

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

import scala.util.parsing.input.{NoPosition, Position}

def TODO(s: String = ""): Nothing = {
    throw new NotImplementedError(s)
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

def AtPosition[T](pos: SourcePosition)(exp: => T): T = try {
    exp
} catch {
    case e: LanguageError =>
        if (e.position == null) e.position = pos
        throw e
}

def isSet[A](collection: Seq[A]): Boolean = collection.toSet.size == collection.size

def counts[A, K](collection: Seq[A], key: A => K): Map[K, Int] = collection.groupBy(key).view.mapValues(_.size).toMap.withDefaultValue(0)
def counts[A](collection: Seq[A]): Map[A, Int] = counts[A, A](collection, identity)

def mapBy[A, K](collection: Seq[A])(f: A => K): Map[K, A] = collection.groupBy(f).view.mapValues(_.head).toMap