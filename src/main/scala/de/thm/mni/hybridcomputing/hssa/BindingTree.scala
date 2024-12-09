package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*

object BindingTree {
    class MultiMap[Key, Value](initialdata: (Key, Value)*) {
        private val data = initialdata.groupBy(_._1).map(kv => kv._1 -> kv._2.map(_._2))
        
        def contains(key: Key): Boolean = this.data.contains(key)
        
        def keys(): Set[Key] = data.keySet
        
        def entries(): Seq[(Key, Seq[Value])] = data.toSeq
        
        def getAll(key: Key): Seq[Value] = this.data.get(key).map(_.toSeq).getOrElse(Seq())
        
        def get(key: Key, filter: Value => Boolean = _ => true): Option[Value] = this.data.get(key).map(_.toSeq).getOrElse(Seq()).find(filter)
    }
    
    def init(program: Syntax.Program) = Program(program)
    
    class Program(val syntax: Syntax.Program) {
        val relations: Seq[GlobalRelationVariable] = syntax.definitions.map(rel => {
            GlobalRelationVariable(rel.name.name, this, Relation(this, rel))
        })
        
        private val entries: MultiMap[String, GlobalRelationVariable | GlobalBuiltinVariable] = MultiMap(
            syntax.language.builtins.map(b => b.name -> GlobalBuiltinVariable(b.name, this, b))
              ++ relations.map(rel => {
                rel.name.name -> rel
            }) *
        )
        
        def getAll(name: String): Seq[GlobalVariable] = entries.getAll(name)
        
        def getRelation(name: String): Option[GlobalRelationVariable] = entries.get(name, _.isInstanceOf[GlobalRelationVariable]).map(_.asInstanceOf[GlobalRelationVariable])
        
        def get(name: String): Option[GlobalVariable] = entries.getAll(name).headOption
        
        def names(): Set[String] = this.entries.keys()
        
        def lookup_variable(name: String): Option[Variable] = this.entries.get(name)
    }
    
    class Relation(val parent: Program, val syntax: Syntax.Relation) {
        val parameter_variables = MultiMap(syntax.parameter.variables.map(v => v.name -> v) *)
        
        val blocks: Seq[Block] = syntax.blocks.map(block => Block(this, block))
        
        private val entries: MultiMap[String, Relation.LabelUsage] = MultiMap(blocks.flatMap(block => block.syntax.entry.labels.zipWithIndex.map(l => l._1.name -> Relation.LabelUsage(l._2, block))) *)
        private val exits: MultiMap[String, Relation.LabelUsage] = MultiMap(blocks.flatMap(block => block.syntax.exit.labels.zipWithIndex.map(l => l._1.name -> Relation.LabelUsage(l._2, block))) *)
        
        lazy val labels: Set[String] = entries.keys() ++ exits.keys()
        
        def getEntryByLabel(label: String): Option[Relation.LabelUsage] = entries.get(label)
        def getExitByLabel(label: String): Option[Relation.LabelUsage] = exits.get(label)
        
        def getAllEntries(label: String): Seq[Relation.LabelUsage] = entries.getAll(label)
        def getAllExits(label: String): Seq[Relation.LabelUsage] = exits.getAll(label)
        
        def lookup_variable(name: String): Option[Variable] = {
            if this.parameter_variables.contains(name) then Some(ParameterVaiable(name, this))
            else this.parent.lookup_variable(name)
        }
    }
    
    object Relation {
        case class LabelUsage(index: Int, block: Block)
        
        enum LabelRole:
            case Entry
            case Exit
    }
    
    class Block(val parent: Relation, val syntax: Syntax.Block) {
        val initializations = MultiMap(
            syntax.sequence.zipWithIndex.flatMap({ case (s, index) => s.initializes.variables.map(v => Block.VariableUsage(v, s, index, Block.VariableRole.Init)) })
              .map(u => u.variable.name.name -> u) *
        )
        
        val usages = MultiMap(
            syntax.sequence.zipWithIndex.flatMap({ case (s, index) => s.uses.variables.map(v => Block.VariableUsage(v, s, index, Block.VariableRole.Use)) })
              .map(u => u.variable.name.name -> u) *
        )
        
        val finalizations = MultiMap(
            syntax.sequence.zipWithIndex.flatMap({ case (s, index) => s.finalizes.variables.map(v => Block.VariableUsage(v, s, index, Block.VariableRole.Final)) })
              .map(u => u.variable.name.name -> u) *
        )
        
        val all_variable_usages: MultiMap[String, Block.VariableUsage] = MultiMap(
            syntax.sequence.zipWithIndex.flatMap({
                  case (s, index) =>
                      s.initializes.variables.map(v => Block.VariableUsage(v, s, index, Block.VariableRole.Init)) ++
                        s.uses.variables.map(v => Block.VariableUsage(v, s, index, Block.VariableRole.Use)) ++
                        s.finalizes.variables.map(v => Block.VariableUsage(v, s, index, Block.VariableRole.Final))
              })
              .map(u => u.variable.name.name -> u) *
        )
        
        val block_local_variables: Set[String] = initializations.keys() ++ finalizations.keys()
        
        def lookup_variable(name: String): Option[Variable] = {
            if this.block_local_variables.contains(name) then Some(BlockVariable(name, this))
            else this.parent.lookup_variable(name)
        }
    }
    
    object Block {
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
    
    class Variable(val name: String)
    case class BlockVariable(override val name: String, val block: Block) extends Variable(name)
    case class ParameterVaiable(override val name: String, val relation: Relation) extends Variable(name)
    abstract class GlobalVariable(name: String, val program: Program) extends Variable(name)
    case class GlobalRelationVariable(override val name: String, override val program: Program, relation: BindingTree.Relation) extends GlobalVariable(name, program)
    case class GlobalBuiltinVariable(override val name: String, override val program: Program, builtin: Language.Plugin.Builtin) extends GlobalVariable(name, program)
}