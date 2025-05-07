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
import scala.collection.mutable
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Method
import de.thm.mni.hybridcomputing.hssa.Syntax.Statement
import de.thm.mni.hybridcomputing.hssa.util.BlockBuilder
import scala.runtime.stdLibPatches.language.experimental.genericNumberLiterals

object Translation {

    def translateRooplToHssa(program: ScopeTree.Program, language: Language): de.thm.mni.hybridcomputing.hssa.Syntax.Program = {
        // Only do main method for now. Later this will have to be done for all methods of all classes
        val main = Relation("main", Expression.Unit())
        Generator.generateRelation(main, program.mainMethod)

        de.thm.mni.hybridcomputing.hssa.Syntax.Program(Seq(main.builder.compile()), language)
    }

    class Relation(val name: String, val parameter: Expression) {
        val builder: RelationBuilder = RelationBuilder(name, parameter, Seq())
        val locals: mutable.Stack[Expression] = mutable.Stack[Expression]()
        val tempVars = UniqueNameGenerator(".")
        // Keeps the currently unfinished block, the first block always entries with "begin"
        var blockBuilder = BlockBuilder((parameter, 0) :=<- "begin")

        def nextTempVar(): String = tempVars.next("_t")
        def nextJumpVar(): String = tempVars.next("_j")
        def nextLabel(): String = builder.label_generator.next("L")
        // Use to end the current block and start a new one
        def nextBlock(exit: Exit, entry: Entry): Unit = {
            builder.add(AutoSSA.autoSSA(blockBuilder.finish(exit)))
            blockBuilder = BlockBuilder(entry)
        }
        def localsForJump(): Expression = {
            // Implicit conversion into pairs using HssaDSL
            locals.reduce((a, b) => (a,b))
        }
    }

    private object Generator {
        var relation: Relation = null

        def generateRelation(relation: Relation, method: Method): Unit = {
            this.relation = relation
            method.body.foreach(generateStatement(_))

            // TODO: Handle parameters properly
            relation.builder.add(AutoSSA.autoSSA(relation.blockBuilder.finish(->("end") := (relation.parameter, 0))))
        }

        private def generateStatement(statement: ScopeTree.StatementNode): Unit = {
            statement match
                case conditional: ScopeTree.Conditional => generateConditional(conditional)
                case loop: ScopeTree.Loop => generateLoop(loop)
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
        private def generateBlock(block: ScopeTree.Block): Unit = {
            // Add variable to locals, so it can be propagated between sub-blocks
            relation.locals.push(block.variable)

            val (compute, tempVar, uncomputeCompute) = generateExpression(block.varCompute)
            val computeVar = block.variable :== ("id", ()) := tempVar

            relation.blockBuilder.addAssignments(compute ++ computeVar)
            block.body.foreach(generateStatement(_))

            val (uncompute, tempVar2, uncomputeUncompute) = generateExpression(block.varUncompute)
            val uncomputeVar = () :== (~"dup", tempVar2) := block.variable

            relation.blockBuilder.addAssignments(uncompute ++ uncomputeVar ++ uncomputeUncompute)
            // Pop variable from locals
            relation.locals.pop()
        }
    
        private def generateConditional(conditional: ScopeTree.Conditional): Unit = {
            val (compute, tempVar, uncompute) = generateExpression(conditional.test)
            val truthVar = relation.nextTempVar()
            val truth = truthVar :== ("equal", (tempVar, 0)) := ()
            val (thenLabel, elseLabel) = (relation.nextLabel(), relation.nextLabel())

            relation.blockBuilder.addAssignments(
                compute ++ truth ++ uncompute
            )
            relation.nextBlock(->(thenLabel, elseLabel) := (relation.localsForJump(), truthVar),
                                (relation.localsForJump(), 0) :=<- thenLabel)
            conditional.thenStatements.foreach(generateStatement(_))

            val (thenJump, elseJump) = (relation.nextLabel(), relation.nextLabel())
            relation.nextBlock(->(thenJump) := (relation.localsForJump(), 0),
                                (relation.localsForJump(), 0) :=<- elseLabel)
            conditional.elseStatements.foreach(generateStatement(_))

            val jumpVar = relation.nextJumpVar()
            relation.nextBlock(->(elseJump) := (relation.localsForJump(), 0),
                                (relation.localsForJump(), jumpVar) :=<- Seq(thenJump, elseJump))

            val (computeAssertion, tempVarAssertion, uncomputeAssertion) = generateExpression(conditional.assertion)
            val assertion = () :== (~"equal", (tempVarAssertion, 0)) := jumpVar
            relation.blockBuilder.addAssignments(
                computeAssertion ++ assertion ++ uncomputeAssertion
            )
        }

        private def generateLoop(loop: ScopeTree.Loop): Unit = {

        }

        private def generateAssignment(assignment: ScopeTree.Assignment): Unit = {
            val (compute, tempVar, uncompute) = generateExpression(assignment.value)
            // TODO: Arrays
            relation.blockBuilder.addAssignments(
                compute ++ (assignment.assignee.name :== (convert(assignment.op), tempVar) := assignment.assignee.name) ++ uncompute
            )
        }

        private def generateSwap(swap: ScopeTree.Swap): Unit = {
            // TODO: Arrays
            relation.blockBuilder.addAssignment(
                (swap.left.name, swap.right.name) :== ("id", ()) := (swap.right.name, swap.left.name)
            )
        }

        private def convert(op: Syntax.AssignmentOperator): String = {
            op match
                case AssignmentOperator.ADD => "add"
                case AssignmentOperator.SUB => "minus"
                case AssignmentOperator.XOR => "xor"
        }

        private def convert(op: Syntax.Operator): String = {
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
        private def generateExpression(expression: ScopeTree.Expression): (Seq[Assignment], String, Seq[Assignment]) = {
            val temp = relation.nextTempVar()
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
}
