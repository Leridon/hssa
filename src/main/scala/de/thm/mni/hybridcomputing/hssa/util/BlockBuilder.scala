package de.thm.mni.hybridcomputing.hssa.util

import de.thm.mni.hybridcomputing.hssa.Syntax.Entry
import de.thm.mni.hybridcomputing.hssa.Syntax.Assignment
import de.thm.mni.hybridcomputing.hssa.Syntax.Exit
import de.thm.mni.hybridcomputing.hssa.Syntax.Block

class BlockBuilder(val entry: Entry) {
    var statements: Seq[Assignment] = Seq()

    def addAssignments(assignments: Seq[Assignment]): Unit = {
        statements = statements ++ assignments
    }

    def addAssignment(assignment: Assignment): Unit = {
        statements = statements :+ assignment
    }

    def finish(exit: Exit): Block = {
        Block(entry, statements, exit)
    }
}