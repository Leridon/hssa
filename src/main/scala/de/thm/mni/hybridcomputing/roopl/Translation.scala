package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.hssa.Language
import de.thm.mni.hybridcomputing.hssa.util.RelationBuilder
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.BindingTree.Block
import de.thm.mni.hybridcomputing.hssa.Syntax.Entry
import de.thm.mni.hybridcomputing.hssa.Syntax.Exit
import de.thm.mni.hybridcomputing.hssa.Syntax.Identifier
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.hssa.Syntax.Assignment

object Translation {
    def translateRooplToHssa(program: ScopeTree.Program, language: Language): de.thm.mni.hybridcomputing.hssa.Syntax.Program = {
        val mainRelation = RelationBuilder("main", Expression.Unit(), Seq())

        mainRelation.add(de.thm.mni.hybridcomputing.hssa.Syntax.Block.apply(
            Entry(Expression.Pair(Expression.Unit(), Expression.Literal(Basic.Int.apply(0))), Seq(Identifier("begin"))),
            program.mainMethod.body.flatMap(generateStatement(_)),
            Exit(Seq(Identifier("end")), Expression.Pair(Expression.Unit(), Expression.Literal(Basic.Int.apply(0))))))

        de.thm.mni.hybridcomputing.hssa.Syntax.Program(Seq(mainRelation.compile()), language)
    }

    def generateStatement(statement: ScopeTree.StatementNode): Seq[Assignment] = {
        statement match
            case ScopeTree.Conditional(test, thenStatements, elseStatements, assertion) => ???
            case ScopeTree.Loop(test, doStatements, loopStatements, assertion) => ???
            case assignment: ScopeTree.Assignment => generateAssignment(assignment)
            case ScopeTree.Swap(left, right) => ???
            case ScopeTree.New(typ, name) => ???
            case ScopeTree.Delete(typ, name) => ???
            case ScopeTree.Copy(typ, from, to) => ???
            case ScopeTree.Uncopy(typ, from, to) => ???
            case ScopeTree.Call(callee, method, args) => ???
            case ScopeTree.Uncall(callee, method, args) => ???
            case block: ScopeTree.Block => ???
    }

    def generateAssignment(assignment: ScopeTree.Assignment): Seq[Assignment] = {
        ???
    }

    def generateExpression(expression: ScopeTree.Expression): (Seq[Assignment], Identifier) = {
        ???
    }

}
