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
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
import de.thm.mni.hybridcomputing.util.UniqueNameGenerator
import de.thm.mni.hybridcomputing.hssa.Inversion.Local.invert
import de.thm.mni.hybridcomputing.roopl.Syntax.AssignmentOperator
import de.thm.mni.hybridcomputing.roopl.Syntax.Operator
import de.thm.mni.hybridcomputing.hssa.transformation.repairs.AutoSSA
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Variable
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import de.thm.mni.hybridcomputing.hssa.transformation.repairs.EliminateImplicitNondeterminism.AutoDiscard

object Translation {

    val tempVars = UniqueNameGenerator(".")
    def nextTempVar() = tempVars.next("_t")

    def translateRooplToHssa(program: ScopeTree.Program, language: Language): de.thm.mni.hybridcomputing.hssa.Syntax.Program = {
        val mainRelation = RelationBuilder("main", Expression.Unit(), Seq())

        // AutoSSA makes it so we don't have to track variable usages ourselves
        mainRelation.add(AutoDiscard.apply(AutoSSA.autoSSA(block(
            ((), 0) :=<- "begin",
            program.mainMethod.body.flatMap(generateStatement(_)),
            ->("end") := ((), 0)
        ))))

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
            case block: ScopeTree.Block => generateBlock(block)
    }

    given Expressionable[Variable] with
            def toExpression(v: Variable): de.thm.mni.hybridcomputing.hssa.Syntax.Expression = v.name.name
    
    given Expressionable[Syntax.VariableIdentifier] with
            def toExpression(v: Syntax.VariableIdentifier): de.thm.mni.hybridcomputing.hssa.Syntax.Expression = v.name
    
    // Compute variable, do body, uncompute variable and garbage
    def generateBlock(block: ScopeTree.Block): Seq[Assignment] = {
        val (compute, tempVar, uncomputeCompute) = generateExpression(block.varCompute)
        val computeVar = block.variable :== ("id", ()) := tempVar
        val body = block.body.flatMap(s => generateStatement(s))
        val (uncompute, tempVar2, uncomputeUncompute) = generateExpression(block.varUncompute)
        val uncomputeVar = () :== (~"dup", tempVar2) := block.variable

        compute ++ computeVar ++ body ++ uncompute ++ uncomputeVar ++ uncomputeUncompute
    }

    def generateAssignment(assignment: ScopeTree.Assignment): Seq[Assignment] = {
        val (compute, tempVar, uncompute) = generateExpression(assignment.value)
        compute ++ (assignment.assignee.name :== (convert(assignment.op), tempVar) := assignment.assignee.name) ++ uncompute
    }

    def convert(op: Syntax.AssignmentOperator): String = {
        op match
            case AssignmentOperator.ADD => "add"
            case AssignmentOperator.SUB => "minus"
            case AssignmentOperator.XOR => "xor"
    }

    def convert(op: Syntax.Operator): String = {
        op match
            case Operator.ADD => "add"
            case Operator.SUB => "minus"
            case Operator.XOR => "xor"
            case Operator.MUL => "mul"
            case Operator.DIV => "div"
            case Operator.MOD => "mod"
            case Operator.BITAND => "and"
            case Operator.BITOR => "or"
            case Operator.LOGAND => "and"
            case Operator.LOGOR => "or"
            case Operator.LESSTHAN => "less"
            case Operator.GREATERTHAN => "greater"
            case Operator.EQUAL => "equal"
            case Operator.NOTEQUAL => "notequal"
            case Operator.LESSEQUAL => "lessequal"
            case Operator.GREATEREQUAL => "greaterequal"
    }

    // Compute expression into temporary variable and return it
    def generateExpression(expression: ScopeTree.Expression): (Seq[Assignment], String, Seq[Assignment]) = {
        val temp = nextTempVar()
        expression match
            case ScopeTree.Expression.Literal(value) =>
                val assignment = temp :== ("add", value) := 0
                (assignment, temp, invert(assignment))
            case ScopeTree.Expression.Reference(ref) =>
                // TODO: Arrays
                val assignment = temp :== ("dup", ref.name) := ()
                (assignment, temp, invert(assignment))
            // How do we represent Nil?
            case ScopeTree.Expression.Nil =>
                val assignment = temp :== ("id", ()) := ()
                (assignment, temp, invert(assignment))
            case ScopeTree.Expression.Binary(left, op, right) =>
                val (computeL, lVar, uncomputeL) = generateExpression(left)
                val (computeR, rVar, uncomputeR) = generateExpression(right)

                val res: Assignment = op match
                    case Operator.ADD | Operator.SUB | Operator.XOR =>
                        temp :== (convert(op), lVar) := rVar

                    case _ => temp :== (convert(op), (lVar, rVar)) := ()
                
                (computeL ++ computeR ++ res, temp, invert(res) ++ uncomputeR ++ uncomputeL)
    }

}
