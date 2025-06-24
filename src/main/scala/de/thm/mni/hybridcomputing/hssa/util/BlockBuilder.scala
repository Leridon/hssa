package de.thm.mni.hybridcomputing.hssa.util

import de.thm.mni.hybridcomputing.hssa.Syntax.Entry
import de.thm.mni.hybridcomputing.hssa.Syntax.Assignment
import de.thm.mni.hybridcomputing.hssa.Syntax.Exit
import de.thm.mni.hybridcomputing.hssa.Syntax.Block
import de.thm.mni.hybridcomputing.hssa.transformation.repairs.AutoSSA

class BlockBuilder(val relation: RelationBuilder, val entry: Entry) {
    var statements: Seq[Assignment] = Seq()

    def adds(assignments: (Seq[Assignment] | Assignment)*): Unit = {
        statements = statements ++ assignments.flatMap {
            case s: Assignment => Seq(s)
            case seq: Seq[_] => seq.asInstanceOf[Seq[Assignment]]
        }
    }

    def add(assignment: Assignment): Unit = {
        statements = statements :+ assignment
    }

    def finish(exit: Exit): Unit = {
        relation.add(AutoSSA.apply(Block(entry, statements, exit)))
    }
}