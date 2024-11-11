package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa

import scala.collection.mutable


object StaticEnvironment {
    def init(language: Language) = SymbolTabl(language)
    
    class SymbolTabl(language: Language) {
        private val entries: mutable.Map[String, SymbolTabl.RelationSymbol | SymbolTabl.BuiltinSymbol] = mutable.Map(
            language.builtins.map(b => b.name -> SymbolTabl.BuiltinSymbol(b)) *
        )
        
        def getRelation(name: String): Option[SymbolTabl.RelationSymbol] = ???
        def get(name: String): Option[SymbolTabl.VarSymbol] = ???
        
        def add(relation: Syntax.Relation): Option[SymbolTabl.RelationSymbol] = {
            if (this.entries.contains(relation.name)) return None
            
            val sym = SymbolTabl.RelationSymbol(relation, RelLocalTable(this))
            
            this.entries.addOne(relation.name -> sym)
            
            Some(sym)
        }
    }
    
    class RelLocalTable(parent: SymbolTabl) {
        def getBlockByEntryLabel(label: String): Syntax.Block = ???
        def getBlockByExitLabel(label: String): Syntax.Block = ???
        
        def getBlocksByEntryLabel(label: String): Seq[Syntax.Block] = ???
        def getBlocksByExitLabel(label: String): Seq[Syntax.Block] = ???
    }
    
    
    class BlockLocalTable(parent: RelLocalTable) {
    
    }
    
    object SymbolTabl {
        sealed trait VarSymbol
        case class RelationSymbol(relation: Syntax.Relation, localContext: RelLocalTable) extends VarSymbol
        case class BuiltinSymbol(builtin: Language.Plugin.Builtin) extends VarSymbol
        case class VariableSymbol(variable: Syntax.Expression.Variable) extends VarSymbol
        case class BlockSymbol(block: Syntax.Block, localTable: BlockLocalTable)
    }
    
}


class SymbolTable[T](
                      override val `type`: SymbolTable.ScopeType,
                      val parent: Option[SymbolTable[T]],
                      val entries: mutable.Map[String, SymbolTable.Symbol[T]],
                      val subscopes: mutable.ListBuffer[(Seq[String | Int], SymbolTable[T])]
                    ) extends SymbolTable.View[T] {
    
    def lookup(name: String): Option[SymbolTable.Symbol[T]] = this.entries.get(name).orElse(this.parent.flatMap(_.lookup(name)))
    
    def getOrCreate(name: String, value: => T): SymbolTable.Symbol[T] = {
        this.entries.getOrElseUpdate(name,
            new SymbolTable.Symbol[T](name, this, value, None, None)
        )
    }
    
    def add(entries: (String, T)*): Unit = {
        this.entries.addAll(entries.map(s => s._1 -> hssa.SymbolTable.Symbol(s._1, this, s._2, None, None)))
    }
    
    def addInitialization(name: String, value: => T, index: Int): Unit = {
        val symbol = this.getOrCreate(name, value)
        
        symbol.initialized_index = Some(index)
    }
    
    def addFinalization(name: String, value: => T, index: Int): Unit = {
        val symbol = this.getOrCreate(name, value)
        
        symbol.finalized_index = Some(index)
    }
    
    def addSubScope(`type`: SymbolTable.ScopeType, names: (String | Int)*): SymbolTable[T] = {
        val scope = new SymbolTable(`type`, Some(this), mutable.Map(), mutable.ListBuffer())
        
        this.subscopes.addOne(names.toSeq -> scope)
        
        scope
    }
    override def getSubContext(name: String): Option[SymbolTable[T]] = this.subscopes.find(_._1.contains(name)).map(_._2)
    def copy(): SymbolTable[T] = {
        new SymbolTable[T](
            this.`type`,
            this.parent,
            this.entries,
            this.subscopes
        )
    }
}

object SymbolTable {
    
    trait View[+T] {
        def `type`: ScopeType
        
        def get(name: String): SymbolTable.Symbol[T] = this.lookup(name).getOrElse({
            throw HSSAError.notFound(name)
        })
        
        def lookup(name: String): Option[SymbolTable.Symbol[T]]
        
        def getSubContext(name: String): Option[SymbolTable.View[T]]
    }
    
    def init[T](): SymbolTable[T] = new SymbolTable[T](ScopeType.Global, None, mutable.Map(), mutable.ListBuffer())
    
    class Symbol[+T](val name: String, val scope: View[T], val value: T, var initialized_index: Option[Int], var finalized_index: Option[Int])
    
    enum ScopeType {
        case Global
        case Proc
        case Block
    }
}