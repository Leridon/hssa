package de.thm.mni.hybridcomputing.hssa.util

import de.thm.mni.hybridcomputing.hssa.Syntax.Entry
import de.thm.mni.hybridcomputing.hssa.Syntax.Assignment
import de.thm.mni.hybridcomputing.hssa.Syntax.Exit
import de.thm.mni.hybridcomputing.hssa.Syntax.Block

class BlockBuilder(val entry: Entry) {
    var statements: Seq[Assignment] = Seq()

    def addAssignments(assignments: (Seq[Assignment] | Assignment)*): Unit = {
        statements = statements ++ assignments.flatMap {
            case s: Assignment => Seq(s)
            case seq: Seq[_] => seq.asInstanceOf[Seq[Assignment]]
        }
    }

    def addAssignment(assignment: Assignment): Unit = {
        statements = statements :+ assignment
    }

    def finish(exit: Exit): Block = {
        Block(entry, statements, exit)
    }
}