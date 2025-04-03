package de.thm.mni.hybridcomputing.util

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class UniqueNameGenerator(separator: String = ".") {
    private val reserved_functions = new ListBuffer[String => Boolean]()
    private val reserved = mutable.Set[String]()
    
    def withExternalReservation(f: String => Boolean): this.type = {
        this.reserved_functions.addOne(f)
    
        this
    }
    
    def isReserved(name: String): Boolean = this.reserved.contains(name) || reserved_functions.exists(f => f(name))
    
    def isAvailable(name: String): Boolean = !this.isReserved(name)
    
    def next(name: String): String = {
        val split = UniqueNameGenerator.splitName(name, this.separator)
        
        val generated_name = LazyList.from(split.postfix.map(_ + 1).getOrElse(0))
          .map(i => UniqueNameGenerator.SplitName(split.prefix, this.separator, Some(i)))
          .find(n => !this.isReserved(n.combined))
          .map(_.combined)
          .get
        
        this.reserved.addOne(generated_name)
        
        generated_name
    }
    
    /**
     * Resets the internal cache of generated names, making them available again.
     * Keeps the function buffer for reserved names sourced externally.
     *
     * Use when all generated names have been integrated in the reserved functions in any way.
     */
    def reset(): Unit = {
        this.reserved.clear()
    }
}

object UniqueNameGenerator {
    case class SplitName(prefix: String, separator: String, postfix: Option[Int]) {
        lazy val combined: String = postfix match
            case Some(value) => prefix + separator + value
            case None => prefix
    }
    
    def splitName(name: String, separator: String = "."): SplitName = {
        val version_string = name.reverse.takeWhile(_.isDigit).reverse
        
        if (version_string.isEmpty) return SplitName(name, separator, None)
        
        val without_version = name.dropRight(version_string.length)
        
        if (!without_version.endsWith(separator)) return SplitName(name, separator, None)
        
        SplitName(without_version.dropRight(separator.length), separator, Some(version_string.toInt))
    }
}
