package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.util.MultiMap.*

trait BindingTree {
    def root: BindingTree.Program
    def lookup(name: String): Option[BindingTree.Variable]
    
    final def program: Syntax.Program = this.root.syntax
}

object BindingTree {
    def init(program: Syntax.Program) = Program(program)
    
    class Program(val syntax: Syntax.Program) extends BindingTree {
        val builtins: Seq[GlobalBuiltinVariable] = syntax.language.builtins.map(b => GlobalBuiltinVariable(b.value.name, this, b))
        
        val relations: Seq[GlobalRelationVariable] = syntax.definitions.map(rel => {
            GlobalRelationVariable(rel.name.name, this, Relation(this, rel))
        })
        
        private val entries: MultiMap[String, GlobalRelationVariable | GlobalBuiltinVariable] = MultiMap(
            builtins.map(b => b.name -> b)
              ++ relations.map(rel => {
                rel.name.name -> rel
            }) *
        )
        
        def getAll(name: String): Seq[GlobalVariable] = entries.getAll(name)
        
        def getRelation(name: String): Option[GlobalRelationVariable] = entries.get(name).filter(_.isInstanceOf[GlobalRelationVariable]).map(_.asInstanceOf[GlobalRelationVariable])
        
        def get(name: String): Option[GlobalVariable] = entries.getAll(name).headOption
        
        def names(): Set[String] = this.entries.keySet
        
        def lookup(name: String): Option[Variable] = this.entries.getFirst(name)
        override def root: Program = this
    }
    
    class Relation(val parent: Program, val syntax: Syntax.Relation) extends BindingTree {
        val parameter_variables: MultiMap[Syntax.Identifier, Expression.Variable] = MultiMap(syntax.parameter.variables.map(v => v.name -> v) *)
        
        val blocks: Seq[Block] = syntax.blocks.zipWithIndex.map({ case (block, i) => Block(Some(Block.Context(this, i)), block) })
        
        private val entries: MultiMap[String, Relation.LabelUsage] = MultiMap(blocks.flatMap(block => block.syntax.entry.labels.zipWithIndex.map(l => l._1.name -> Relation.LabelUsage(l._2, block, Relation.LabelRole.Entry))) *)
        private val exits: MultiMap[String, Relation.LabelUsage] = MultiMap(blocks.flatMap(block => block.syntax.exit.labels.zipWithIndex.map(l => l._1.name -> Relation.LabelUsage(l._2, block, Relation.LabelRole.Exit))) *)
        
        lazy val labels: Set[Label] = (entries.keySet ++ exits.keySet).map(l => Label(l, this))
        
        def getEntryByLabel(label: String): Option[Relation.LabelUsage] = entries.getFirst(label)
        def getExitByLabel(label: String): Option[Relation.LabelUsage] = exits.getFirst(label)
        
        def getAllEntries(label: String): Seq[Relation.LabelUsage] = entries.getAll(label)
        def getAllExits(label: String): Seq[Relation.LabelUsage] = exits.getAll(label)
        
        def lookup(name: String): Option[Variable] = {
            if this.parameter_variables.contains(name) then Some(ParameterVariable(name, this))
            else this.parent.lookup(name)
        }
        
        override def root: Program = this.parent
        
        override def equals(obj: Any): Boolean = obj match {
            case r: Relation => r eq this
            case _ => false
        }
    }
    
    object Relation {
        case class LabelUsage(index: Int, block: Block, role: LabelRole)
        
        enum LabelRole:
            case Entry
            case Exit
    }
    
    class Block(val context: Option[Block.Context], val syntax: Syntax.Block) extends BindingTree {
        val entry_labels: Seq[Label] = syntax.entry.labels.map(l => Label(l.name, context.map(_.relation).orNull))
        val exit_labels: Seq[Label] = syntax.exit.labels.map(l => Label(l.name, context.map(_.relation).orNull))
        
        val initializations: MultiMap[String, Block.VariableUsage] = MultiMap(
            syntax.sequence.zipWithIndex.flatMap({ case (s, index) => s.initializes.variables.map(v => Block.VariableUsage(this, v, s, index, Block.VariableRole.Init)) })
              .map(u => u.variable.name.name -> u) *
        )
        
        val usages: MultiMap[String, Block.VariableUsage] = MultiMap(
            syntax.sequence.zipWithIndex.flatMap({ case (s, index) => s.uses.variables.map(v => Block.VariableUsage(this, v, s, index, Block.VariableRole.Use)) })
              .map(u => u.variable.name.name -> u) *
        )
        
        val finalizations: MultiMap[String, Block.VariableUsage] = MultiMap(
            syntax.sequence.zipWithIndex.flatMap({ case (s, index) => s.finalizes.variables.map(v => Block.VariableUsage(this, v, s, index, Block.VariableRole.Final)) })
              .map(u => u.variable.name.name -> u) *
        )
        
        val all_variable_usages: MultiMap[String, Block.VariableUsage] = MultiMap(
            syntax.sequence.zipWithIndex.flatMap({
                  case (s, index) =>
                      s.initializes.variables.map(v => Block.VariableUsage(this, v, s, index, Block.VariableRole.Init)) ++
                        s.uses.variables.map(v => Block.VariableUsage(this, v, s, index, Block.VariableRole.Use)) ++
                        s.finalizes.variables.map(v => Block.VariableUsage(this, v, s, index, Block.VariableRole.Final))
              })
              .map(u => u.variable.name.name -> u) *
        )
        
        val block_local_variables: Set[String] = initializations.keySet ++ finalizations.keySet
        
        def lookup(name: String): Option[Variable] = {
            if this.block_local_variables.contains(name) then Some(BlockVariable(name, this))
            else this.context.flatMap(_.relation.lookup(name))
        }
        
        override def root: Program = this.context.get.relation.root
        
        override def equals(obj: Any): Boolean = obj match {
            case r: Block => r eq this
            case _ => false
        }
    }
    
    case class Label(name: String, relation: Relation)
    
    object Block {
        case class Context(relation: Relation, block_index: Int)
        
        case class VariableUsage(
                                  block: Block,
                                  variable: Syntax.Expression.Variable,
                                  statement: Syntax.Statement,
                                  statement_index: Int,
                                  role: VariableRole
                                ) {
            def ref: Option[Variable] = block.lookup(this.variable.name.name)
        }
        
        enum VariableRole:
            case Init
            case Final
            case Use
    }
    
    sealed class Variable(val name: String)
    case class BlockVariable(override val name: String, block: Block) extends Variable(name)
    case class ParameterVariable(override val name: String, relation: Relation) extends Variable(name)
    
    sealed abstract class GlobalVariable(name: String, val program: Program) extends Variable(name)
    case class GlobalRelationVariable(override val name: String, override val program: Program, relation: BindingTree.Relation) extends GlobalVariable(name, program)
    case class GlobalBuiltinVariable(override val name: String, override val program: Program, builtin: Language.Plugin.Builtin) extends GlobalVariable(name, program)
}