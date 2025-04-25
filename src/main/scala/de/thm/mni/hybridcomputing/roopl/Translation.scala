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
    var relation: RelationBuilder = null
    def nextTempVar() = tempVars.next("_t")
    def nextLabel() = relation.label_generator.next("L")

    def translateRooplToHssa(program: ScopeTree.Program, language: Language): de.thm.mni.hybridcomputing.hssa.Syntax.Program = {
        relation = RelationBuilder("main", Expression.Unit(), Seq())

        // AutoSSA makes it so we don't have to track variable usages ourselves
        relation.add(AutoDiscard.apply(AutoSSA.autoSSA(block(
            ((), 0) :=<- "begin",
            program.mainMethod.body.flatMap(generateStatement(_)),
            ->("end") := ((), 0)
        ))))

        de.thm.mni.hybridcomputing.hssa.Syntax.Program(Seq(relation.compile()), language)
    }

    def generateStatement(statement: ScopeTree.StatementNode): Seq[Assignment] = {
        statement match
            case conditional: ScopeTree.Conditional => generateConditional(conditional)
            case loop: ScopeTree.Loop => ???
            case assignment: ScopeTree.Assignment => generateAssignment(assignment)
            case swap: ScopeTree.Swap => generateSwap(swap)
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

    def generateConditional(conditional: ScopeTree.Conditional): Seq[Assignment] = {
        val (compute, tempVar, uncompute) = generateExpression(conditional.test)
        val truthVar = nextTempVar()
        val truth = truthVar :== ("equal", (tempVar, 0)) := ()
        val (thenLabel, elseLabel) = (nextLabel(), nextLabel())
        val jump = ->(elseLabel, thenLabel) := ((), truthVar)

        compute ++ uncompute ++ truth ++ jump
    }

    def generateAssignment(assignment: ScopeTree.Assignment): Seq[Assignment] = {
        val (compute, tempVar, uncompute) = generateExpression(assignment.value)
        // TODO: Arrays        
        compute ++ (assignment.assignee.name :== (convert(assignment.op), tempVar) := assignment.assignee.name) ++ uncompute
    }

    def generateSwap(swap: ScopeTree.Swap): Seq[Assignment] = {
        // TODO: Arrays
        (swap.left.name, swap.right.name) :== ("id", ()) := (swap.right.name, swap.left.name)
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
                        // This consumes rvar but also returns an uncompute down the line, which leads to double initialization & finalization
                        // This is fixed by AutoSSA which replaces the second occurrences with an entirely new intermediate variable, solving this problem
                        temp :== (convert(op), lVar) := rVar

                    case _ => temp :== (convert(op), (lVar, rVar)) := ()
                
                (computeL ++ computeR ++ res, temp, invert(res) ++ uncomputeR ++ uncomputeL)
    }

}
