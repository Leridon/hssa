package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*

object StaticEnvironment {
    class MultiMap[Key, Value](initialdata: (Key, Value)*) {
        private val data = mutable.Map[Key, ListBuffer[Value]]()
        
        this.add(initialdata *)
        
        def keys(): Set[Key] = data.keySet.toSet
        
        def add(pair: (Key, Value)*): Unit = {
            pair.foreach({
                case (key, value) => this.data.getOrElseUpdate(key, new ListBuffer[Value]()).addOne(value)
            })
        }
        
        def getAll(key: Key): Seq[Value] = this.data.get(key).map(_.toSeq).getOrElse(Seq())
        
        def get(key: Key, filter: Value => Boolean = _ => true): Option[Value] = this.data.get(key).map(_.toSeq).getOrElse(Seq()).find(filter)
        
        def all: Map[Key, Seq[Value]] = this.data.toMap.view.mapValues(_.toSeq).toMap
    }
    
    def init(language: Language, program: Syntax.Program) = SymbolTabl(language, program)
    
    class SymbolTabl(language: Language, program: Syntax.Program) {
        val relations: Seq[SymbolTabl.RelationSymbol] = program.definitions.map(rel => {
            SymbolTabl.RelationSymbol(RelLocalTable(this, rel))
        })
        
        private val entries: MultiMap[String, SymbolTabl.RelationSymbol | SymbolTabl.BuiltinSymbol] = MultiMap(
            language.builtins.map(b => b.name -> SymbolTabl.BuiltinSymbol(b)) *
        )
        
        this.entries.add(
            program.definitions.map(rel => {
                rel.name.name -> SymbolTabl.RelationSymbol(RelLocalTable(this, rel))
            }) *
        )
        
        def getAll(name: String): Seq[SymbolTabl.RelationSymbol | SymbolTabl.BuiltinSymbol] = entries.getAll(name)
        
        def getRelation(name: String): Option[SymbolTabl.RelationSymbol] = entries.get(name, _.isInstanceOf[SymbolTabl.RelationSymbol]).map(_.asInstanceOf[SymbolTabl.RelationSymbol])
        
        def get(name: String): Option[SymbolTabl.VarSymbol] = entries.getAll(name).headOption
        
        def names(): Set[String] = this.entries.keys()
    }
    
    class RelLocalTable(val parent: SymbolTabl, val relation: Syntax.Relation) {
        val blocks: Seq[BlockLocalTable] = relation.blocks.map(block => BlockLocalTable(this, block))
        
        private val entries: MultiMap[String, RelLocalTable.LabelUsage] = MultiMap(blocks.flatMap(block => block.block.entry.labels.zipWithIndex.map(l => l._1.name -> RelLocalTable.LabelUsage(l._2, block))) *)
        private val exits: MultiMap[String, RelLocalTable.LabelUsage] = MultiMap(blocks.flatMap(block => block.block.exit.labels.zipWithIndex.map(l => l._1.name -> RelLocalTable.LabelUsage(l._2, block))) *)
        
        lazy val labels: Set[String] = entries.keys() ++ exits.keys()
        
        def getEntryByLabel(label: String): Option[RelLocalTable.LabelUsage] = entries.get(label)
        def getExitByLabel(label: String): Option[RelLocalTable.LabelUsage] = exits.get(label)
        
        def getAllEntries(label: String): Seq[RelLocalTable.LabelUsage] = entries.getAll(label)
        def getAllExits(label: String): Seq[RelLocalTable.LabelUsage] = exits.getAll(label)
    }
    
    object RelLocalTable {
        case class LabelUsage(index: Int, block: BlockLocalTable)
        
        enum LabelRole:
            case Entry
            case Exit
    }
    
    class BlockLocalTable(val parent: RelLocalTable, val block: Syntax.Block) {
        val initializations = MultiMap(
            block.sequence.zipWithIndex.flatMap({ case (s, index) => s.initializes.variables.map(v => BlockLocalTable.VariableUsage(v, s, index, BlockLocalTable.VariableRole.Init)) })
              .map(u => u.variable.name.name -> u) *
        )
        
        val usages = MultiMap(
            block.sequence.zipWithIndex.flatMap({ case (s, index) => s.uses.variables.map(v => BlockLocalTable.VariableUsage(v, s, index, BlockLocalTable.VariableRole.Use)) })
              .map(u => u.variable.name.name -> u) *
        )
        
        val finalizations = MultiMap(
            block.sequence.zipWithIndex.flatMap({ case (s, index) => s.finalizes.variables.map(v => BlockLocalTable.VariableUsage(v, s, index, BlockLocalTable.VariableRole.Final)) })
              .map(u => u.variable.name.name -> u) *
        )
        
        val all_variable_usages: MultiMap[String, BlockLocalTable.VariableUsage] = MultiMap(
            block.sequence.zipWithIndex.flatMap({
                  case (s, index) =>
                      s.initializes.variables.map(v => BlockLocalTable.VariableUsage(v, s, index, BlockLocalTable.VariableRole.Init)) ++
                        s.uses.variables.map(v => BlockLocalTable.VariableUsage(v, s, index, BlockLocalTable.VariableRole.Use)) ++
                        s.finalizes.variables.map(v => BlockLocalTable.VariableUsage(v, s, index, BlockLocalTable.VariableRole.Final))
              })
              .map(u => u.variable.name.name -> u) *
        )
        
        val block_local_variables: Set[String] = initializations.keys() ++ finalizations.keys()
    }
    
    object BlockLocalTable {
        case class VariableUsage(
                                  variable: Syntax.Expression.Variable,
                                  statement: Syntax.Statement,
                                  statement_index: Int,
                                  role: VariableRole
                                )
        
        enum VariableRole:
            case Init
            case Final
            case Use
    }
    
    object SymbolTabl {
        sealed trait VarSymbol
        case class RelationSymbol(localContext: RelLocalTable) extends VarSymbol
        case class BuiltinSymbol(builtin: Language.Plugin.Builtin) extends VarSymbol
        case class VariableSymbol(variable: Syntax.Expression.Variable) extends VarSymbol
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
            HSSAError.notFound(name).raise()
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