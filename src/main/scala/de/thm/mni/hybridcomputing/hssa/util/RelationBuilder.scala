package de.thm.mni.hybridcomputing.hssa.util

import RelationBuilder.LabelUsage
import RelationBuilder.LabelUsage.Position
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.Syntax.{Relation, Statement}
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.BlockIndex
import de.thm.mni.hybridcomputing.hssa.{Syntax, util}

import scala.collection.mutable.ListBuffer
import scala.util.chaining.*
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.util.UniqueNameGenerator

class RelationBuilder(
                       var name: String,
                       var parameter: Syntax.Expression
                     ) {
    private val blocks: ListBuffer[RelationBuilder.BlockBuilder] = new ListBuffer[RelationBuilder.BlockBuilder]
    
    def this(relation: Syntax.Relation) = {
        this(relation.name.name, relation.parameter)
        
        relation.blocks.foreach(b => this.add(b))
    }
    
    def getByEntryLabel(label: String): RelationBuilder.BlockBuilder = blocks.find(b => b.entry.labels.contains(label)).get
    def getByExitLabel(label: String): RelationBuilder.BlockBuilder = blocks.find(b => b.exit.labels.contains(label)).get
    
    def getAllByEntryLabel(label: String): Seq[RelationBuilder.BlockBuilder] = blocks.filter(b => b.entry.labels.contains(label)).toSeq
    def getAllByExitLabel(label: String): Seq[RelationBuilder.BlockBuilder] = blocks.filter(b => b.exit.labels.contains(label)).toSeq
    
    def labels: Set[String] = this.blocks.flatMap(b => b.entry.labels ++ b.exit.labels).toSet.map(_.name)
    
    def remove(block: RelationBuilder.BlockBuilder): Unit = {
        this.blocks.remove(this.blocks.indexOf(block))
    }
    
    def add(block: Syntax.Block): Unit = {
        this.blocks.addOne(RelationBuilder.BlockBuilder(this, block))
    }
    
    def compile(): Syntax.Relation = Syntax.Relation(name, parameter, this.blocks.map(_.block()).toSeq)
    
    def filterBlocksInPlace(f: Syntax.Block => Boolean): Unit = this.blocks.filterInPlace(b => f(b.block()))
    
    val label_generator: UniqueNameGenerator = new UniqueNameGenerator("")
      .withExternalReservation(name => this.allLabelUsages.exists(_.label == name))
    
    def allLabelUsages: List[RelationBuilder.LabelUsage] = {
        def getUsages(block: RelationBuilder.BlockBuilder, stm: Statement): List[RelationBuilder.LabelUsage] = {
            stm match
                case Syntax.Exit(labels, _) => labels.zipWithIndex.map({ case (l, i) => RelationBuilder.LabelUsage(block, i, LabelUsage.Position.EXIT, l.name) }).toList
                case Syntax.Entry(_, labels) => labels.zipWithIndex.map({ case (l, i) => RelationBuilder.LabelUsage(block, i, LabelUsage.Position.ENTRY, l.name) }).toList
                case _ => Nil
        }
        
        this.blocks.toList.flatMap(b => getUsages(b, b.entry) ++ getUsages(b, b.exit))
    }
    
    def updateLabels(f: RelationBuilder.LabelUsage => String): Unit = {
        this.blocks.foreach(_.updateLabels(f))
    }
}

object RelationBuilder {
    class BlockBuilder(val parent: RelationBuilder, init: Syntax.Block) {
        private var _entry: Syntax.Entry = init.entry
        private var _assignments = new ListBuffer[Syntax.Assignment].tap(_.addAll(init.assignments))
        private var _exit: Syntax.Exit = init.exit
        
        def updateLabels(f: RelationBuilder.LabelUsage => String): Unit = {
            _entry = Syntax.Entry(_entry.initialized, _entry.labels.zipWithIndex.map({ case (l, i) => RelationBuilder.LabelUsage(this, i, LabelUsage.Position.ENTRY, l.name) }).map(u => f(u)))
            _exit = Syntax.Exit(_exit.labels.zipWithIndex.map({ case (l, i) => RelationBuilder.LabelUsage(this, i, LabelUsage.Position.EXIT, l.name) }).map(u => f(u)), _exit.argument)
        }
        
        def block(): Syntax.Block = Syntax.Block(entry, this.assignment, exit)
        
        def entry: Syntax.Entry = _entry
        def assignment: Seq[Syntax.Assignment] = _assignments.toSeq
        def exit: Syntax.Exit = _exit
    }
    
    case class LabelUsage(
                           block: BlockBuilder,
                           pos: Int,
                           role: LabelUsage.Position,
                           label: String
                         ) {
        
        override def equals(obj: Any): Boolean = {
            obj match {
                case other: LabelUsage =>
                    (block eq other.block) &&
                      pos == other.pos &&
                      role == other.role &&
                      label == other.label
                
                case _ => false
            }
        }
        
    }
    
    object LabelUsage {
        enum Position {
            case ENTRY
            case EXIT
        }
    }
}
