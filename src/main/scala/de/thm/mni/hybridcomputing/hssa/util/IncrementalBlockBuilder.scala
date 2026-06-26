package de.thm.mni.hybridcomputing.hssa.util

import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.hssa.Syntax.Entry
import de.thm.mni.hybridcomputing.hssa.Syntax.Assignment
import de.thm.mni.hybridcomputing.hssa.Syntax.Exit
import de.thm.mni.hybridcomputing.hssa.Syntax.Block
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.variables
import de.thm.mni.hybridcomputing.hssa.transformation.repairs.AutoSSA
import de.thm.mni.hybridcomputing.util.UniqueNameGenerator

class IncrementalBlockBuilder(val relation: RelationBuilder, val _entry: Entry) {
    var _statements: Seq[Assignment] = Seq()

    private val variable_generator = new UniqueNameGenerator()
      .withExternalReservation(s => _entry.output.variables.exists(v => v.name.name == s))
      .withExternalReservation(s => _statements.exists(a => a.input.variables.exists(v => v.name.name == s)))
      .withExternalReservation(s => _statements.exists(a => a.output.variables.exists(v => v.name.name == s)))

    def freshVariable(prefix: String): Syntax.Expression.Variable = {
        Syntax.Expression.Variable(Syntax.Identifier(this.variable_generator.next(prefix)))
    }

    def adds(assignments: (Seq[Assignment] | Assignment)*): Unit = {
        _statements = _statements ++ assignments.flatMap {
            case s: Assignment => Seq(s)
            case seq: Seq[_] => seq.asInstanceOf[Seq[Assignment]]
        }
    }

    def add(assignment: Assignment): Unit = {
        _statements = _statements :+ assignment
    }

    def finish(exit: Exit): Unit = {
        relation.add(AutoSSA.apply(Block(_entry, _statements, exit)))
    }

    def finish2(exit: Exit): Unit = {
        relation.add(Block(_entry, _statements, exit))
    }
}