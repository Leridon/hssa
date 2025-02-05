package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.util.MultiMap.*

trait BindingTree {
    def root: BindingTree.Program
    def lookup_variable(name: String): Option[BindingTree.Variable]
    
    final def program: Syntax.Program = this.root.syntax
}

object BindingTree {
        
    def init(program: Syntax.Program) = Program(program)
    
    class Program(val syntax: Syntax.Program) extends BindingTree {
        val relations: Seq[GlobalRelationVariable] = syntax.definitions.map(rel => {
            GlobalRelationVariable(rel.name.name, this, Relation(this, rel))
        })
        
        private val entries: MultiMap[String, GlobalRelationVariable | GlobalBuiltinVariable] = MultiMap(
            syntax.language.builtins.map(b => b.value.name -> GlobalBuiltinVariable(b.value.name, this, b))
              ++ relations.map(rel => {
                rel.name.name -> rel
            }) *
        )
        
        def getAll(name: String): Seq[GlobalVariable] = entries.getAll(name)
        
        def getRelation(name: String): Option[GlobalRelationVariable] = entries.getFirst(name, _.isInstanceOf[GlobalRelationVariable]).map(_.asInstanceOf[GlobalRelationVariable])
        
        def get(name: String): Option[GlobalVariable] = entries.getAll(name).headOption
        
        def names(): Set[String] = this.entries.keySet
        
        def lookup_variable(name: String): Option[Variable] = this.entries.getFirst(name)
        override def root: Program = this
    }
    
    class Relation(val parent: Program, val syntax: Syntax.Relation) extends BindingTree {
        val parameter_variables = MultiMap(syntax.parameter.variables.map(v => v.name -> v) *)
        
        val blocks: Seq[Block] = syntax.blocks.map(block => Block(this, block))
        
        private val entries: MultiMap[String, Relation.LabelUsage] = MultiMap(blocks.flatMap(block => block.syntax.entry.labels.zipWithIndex.map(l => l._1.name -> Relation.LabelUsage(l._2, block))) *)
        private val exits: MultiMap[String, Relation.LabelUsage] = MultiMap(blocks.flatMap(block => block.syntax.exit.labels.zipWithIndex.map(l => l._1.name -> Relation.LabelUsage(l._2, block))) *)
        
        lazy val labels: Set[String] = entries.keySet ++ exits.keySet
        
        def getEntryByLabel(label: String): Option[Relation.LabelUsage] = entries.getFirst(label)
        def getExitByLabel(label: String): Option[Relation.LabelUsage] = exits.getFirst(label)
        
        def getAllEntries(label: String): Seq[Relation.LabelUsage] = entries.getAll(label)
        def getAllExits(label: String): Seq[Relation.LabelUsage] = exits.getAll(label)
        
        def lookup_variable(name: String): Option[Variable] = {
            if this.parameter_variables.contains(name) then Some(ParameterVaiable(name, this))
            else this.parent.lookup_variable(name)
        }
        
        override def root: Program = this.parent
    }
    
    object Relation {
        case class LabelUsage(index: Int, block: Block)
        
        enum LabelRole:
            case Entry
            case Exit
    }
    
    class Block(val parent: Relation, val syntax: Syntax.Block) extends BindingTree {
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
        
        val block_local_variables: Set[String] = initializations.keySet ++ finalizations.keySet
        
        def lookup_variable(name: String): Option[Variable] = {
            if this.block_local_variables.contains(name) then Some(BlockVariable(name, this))
            else this.parent.lookup_variable(name)
        }
        
        override def root: Program = this.parent.root
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