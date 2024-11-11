package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.RelationBuilder.LabelUsage
import de.thm.mni.hybridcomputing.hssa.RelationBuilder.LabelUsage.Position
import de.thm.mni.hybridcomputing.hssa.Syntax.{Relation, Statement}
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.BlockIndex

import scala.collection.mutable.ListBuffer

class RelationBuilder(name: String, parameter: Syntax.Expression, initial_blocks: Seq[Syntax.Block]) {
    def this(relation: Syntax.Relation) = {
        this(relation.name, relation.parameter, relation.blocks)
    }
    
    val blocks: ListBuffer[Syntax.Block] = new ListBuffer[Syntax.Block]
    
    blocks.addAll(initial_blocks)
    
    private val reserved_labels = new ListBuffer[String]
    
    def getByEntryLabel(label: String): Syntax.Block = blocks.find(b => b.entry.labels.contains(label)).get
    def getByExitLabel(label: String): Syntax.Block = blocks.find(b => b.exit.labels.contains(label)).get
    
    def getAllByEntryLabel(label: String): Seq[Syntax.Block] = blocks.filter(b => b.entry.labels.contains(label)).toSeq
    def getAllByExitLabel(label: String): Seq[Syntax.Block] = blocks.filter(b => b.exit.labels.contains(label)).toSeq
    
    def labels: Set[String] = this.blocks.flatMap(b => b.entry.labels ++ b.exit.labels).toSet
    
    def remove(block: Syntax.Block): Unit = {
        this.blocks.remove(this.blocks.indexOf(block))
    }
    
    def add(block: Syntax.Block): Unit = {
        this.blocks.addOne(block)
    }
    
    def compile(): Syntax.Relation = Syntax.Relation(name, parameter, this.blocks.toSeq)
    
    def filterBlocks(f: Syntax.Block => Boolean): Unit = this.blocks.filterInPlace(f)
    
    def newLabel(template: String): String = {
        val raw_template = template.reverse.dropWhile(_.isDigit).reverse
        
        val existing_labels = this.reserved_labels.toSet ++ this.allLabelUsages.map(_.label).toSet
        
        val label = LazyList.from(0).map(i => s"$raw_template$i").find(i => !existing_labels.contains(i)).get
        
        this.reserved_labels.addOne(label)
        
        label
    }
    
    def allLabelUsages: List[RelationBuilder.LabelUsage] = {
        def getUsages(block: Syntax.Block, stm: Statement): List[RelationBuilder.LabelUsage] = {
            stm match
                case Syntax.Assignment(target, relation, instance_argument, source) => Nil
                case Syntax.Exit(labels, argument) => labels.zipWithIndex.map({ case (l, i) => RelationBuilder.LabelUsage(block, i, LabelUsage.Position.EXIT, l) }).toList
                case Syntax.Entry(argument, labels) => labels.zipWithIndex.map({ case (l, i) => RelationBuilder.LabelUsage(block, i, LabelUsage.Position.ENTRY, l) }).toList
        }
        
        this.blocks.toList.flatMap(b => getUsages(b, b.entry) ++ getUsages(b, b.exit))
    }
    
    def updateLabels(f: RelationBuilder.LabelUsage => String): Unit =
        this.blocks.mapInPlace(b => Syntax.Block(
            Syntax.Entry(b.entry.initialized, b.entry.labels.zipWithIndex.map({ case (l, i) => RelationBuilder.LabelUsage(b, i, LabelUsage.Position.ENTRY, l) }).map(f)),
            b.assignments,
            Syntax.Exit(b.exit.labels.zipWithIndex.map({ case (l, i) => RelationBuilder.LabelUsage(b, i, LabelUsage.Position.EXIT, l) }).map(f), b.exit.argument)
        ))
}

object RelationBuilder {
    case class LabelUsage(
                           block: Syntax.Block,
                           pos: Int,
                           role: LabelUsage.Position,
                           label: String
                         ) {}
    
    object LabelUsage {
        enum Position {
            case ENTRY
            case EXIT
        }
    }
}
