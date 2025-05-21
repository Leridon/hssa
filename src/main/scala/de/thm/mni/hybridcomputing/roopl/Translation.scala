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
import de.thm.mni.hybridcomputing.hssa;
import de.thm.mni.hybridcomputing.util.UniqueNameGenerator
import de.thm.mni.hybridcomputing.hssa.Inversion.Local.invert
import de.thm.mni.hybridcomputing.roopl.Syntax.AssignmentOperator
import de.thm.mni.hybridcomputing.roopl.Syntax.Operator
import de.thm.mni.hybridcomputing.hssa.transformation.repairs.AutoSSA
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Variable
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import scala.collection.mutable
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Method
import de.thm.mni.hybridcomputing.hssa.Syntax.Statement
import de.thm.mni.hybridcomputing.hssa.util.BlockBuilder
import scala.runtime.stdLibPatches.language.experimental.genericNumberLiterals
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.VariableReference
import de.thm.mni.hybridcomputing.roopl.wellformedness.Typing

object Translation {
    given Expressionable[Variable] with
        def toExpression(v: Variable): Expression = v.name.name
    
    given Expressionable[Syntax.VariableIdentifier] with
        def toExpression(v: Syntax.VariableIdentifier): Expression = v.name

    given Expressionable[Seq[Variable]] with
        def toExpression(s: Seq[Variable]): Expression = s match
            case Seq() => Expression.Unit()
            case _ => s.map[Expression](identity).reduce((a, b) => (a, b))

    def translateRooplToHssa(program: ScopeTree.Program, language: Language): hssa.Syntax.Program = {
        // Setup managed memory and call roopl main
        val main: RelationBuilder = RelationBuilder("main", ())
        val setup = Seq(
                "m" :== ("mmem.new", ()) := (),
                // Create _this for main
                ("m", "_main") :== ("mmem.allocate", 1) := "m")
        main.add(AutoSSA(hssa.Syntax.Block(
            ((), 0) :=<- "begin",
            setup ++
            (("m", "_main") :== (s"_${program.mainClass.name}.${program.mainMethod.name}", ()) := ("m", "_main")) ++
            invert(setup),
            ->("end") := ((), 0)
        )))

        // Generate program
        val relations: Seq[hssa.Syntax.Relation] = program.classes.flatMap(clazz => {
            clazz.methods.map(method => {
                val relation = Relation(s"_${clazz.name}.${method.name}", method.parameters)
                Generator.generateRelation(relation, method)
                relation.builder.compile()
            })
        }).appended(main.compile())

        hssa.Syntax.Program(relations, language)
    }

    class Relation(val name: String, val parameter: Seq[Variable]) {
        // _this refers to the object on which a method is called
        // All methods have a callee except main, for which a special object is created
        val builder: RelationBuilder = RelationBuilder(name, ())
        val locals: mutable.Stack[Variable] = mutable.Stack[Variable]()
        val tempVars = UniqueNameGenerator(".")
        // Keeps the currently unfinished block, the first block always entries with "begin"
        var blockBuilder = BlockBuilder((Generator.hssaParameters(parameter), 0) :=<- "begin")
        
        def nextTempVar(): String = tempVars.next("_t")
        def nextJumpVar(): String = tempVars.next("_j")
        def nextLabel(): String = builder.label_generator.next("L")
        // Use to end the current block and start a new one
        def nextBlock(exitLabels: Seq[String], exitJump: Expression, entryJump: Expression, entryLabels: Seq[String]): Unit = {
            val exit = ->(exitLabels) := (locals.toSeq, exitJump)
            val entry = (locals.toSeq, entryJump) :=<- entryLabels
            builder.add(AutoSSA.apply(blockBuilder.finish(exit)))
            blockBuilder = BlockBuilder(entry)
            tempVars.reset()
        }
        def nextBlock(exitLabels: String, exitJump: Expression, entryJump: Expression, entryLabels: String): Unit = {
            nextBlock(Seq(exitLabels), exitJump, entryJump, Seq(entryLabels))
        }
        def nextBlock(exitLabels: (String, String), exitJump: Expression, entryJump: Expression, entryLabels: String): Unit = {
            nextBlock(exitLabels.toList, exitJump, entryJump, Seq(entryLabels))
        }
        def nextBlock(exitLabels: String, exitJump: Expression, entryJump: Expression, entryLabels: (String, String)): Unit = {
            nextBlock(Seq(exitLabels), exitJump, entryJump, entryLabels.toList)
        }
        def nextBlock(exitLabels: (String, String), exitJump: Expression, entryJump: Expression, entryLabels: (String, String)): Unit = {
            nextBlock(exitLabels.toList, exitJump, entryJump, entryLabels.toList)
        }
    }
    
    private object Generator {
        var relation: Relation = null
        
        def generateRelation(relation: Relation, method: Method): Unit = {
            this.relation = relation
            method.body.foreach(generateStatement(_))
            
            relation.builder.add(AutoSSA.apply(relation.blockBuilder.finish(->("end") := (hssaParameters(relation.parameter), 0))))
        }

        def hssaParameters(parameters: Seq[Variable]): Expression = {
            parameters match
                case Seq() => ("_this", "_m")
                case _ => (("_this", "_m"), parameters)
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
                case ScopeTree.Call(callee, method, args) => generateCall(callee, method, args, false)
                case ScopeTree.Uncall(callee, method, args) => generateCall(callee, method, args, false)
                case block: ScopeTree.Block => generateBlock(block)
        }
        
        // Compute variable, do body, uncompute variable and garbage
        private def generateBlock(block: ScopeTree.Block): Unit = {
            // Add variable to locals, so it can be propagated between sub-blocks
            relation.locals.push(block.variable)
            
            val (local_compute, local_temp_var) = generateExpression(block.varCompute)
            
            relation.blockBuilder.addAssignments(
                local_compute,
                block.variable :== ("dup", local_temp_var) := (),
                invert(local_compute) // We need to clean up any potential temporaries left behind
            )
            
            block.body.foreach(generateStatement)
            
            val (delocal_compute, delocal_temp_var) = generateExpression(block.varUncompute)
            
            relation.blockBuilder.addAssignments(
                delocal_compute,
                () :== (~"dup", delocal_temp_var) := block.variable,
                invert(delocal_compute)
            )
            // Pop variable from locals
            relation.locals.pop()
        }
        
        private def generateConditional(conditional: ScopeTree.Conditional): Unit = {
            // Check condition
            val (computeTest, tempVar) = generateExpression(conditional.test)
            val truthVar = relation.nextTempVar()
            val truth = truthVar :== ("equal", (tempVar, 0)) := ()
            val (thenLabel, elseLabel) = (relation.nextLabel(), relation.nextLabel())
            
            relation.blockBuilder.addAssignments(
                computeTest, truth, invert(computeTest)
            )
            // Jump to then or else
            relation.nextBlock((thenLabel, elseLabel), truthVar,
                0, thenLabel)
            // Then
            conditional.thenStatements.foreach(generateStatement(_))
            
            // Else
            val (thenJump, elseJump) = (relation.nextLabel(), relation.nextLabel())
            relation.nextBlock((thenJump), 0,
                0, elseLabel)
            conditional.elseStatements.foreach(generateStatement(_))
            
            val jumpVar = relation.nextJumpVar()
            relation.nextBlock((elseJump), 0,
                jumpVar, (thenJump, elseJump))
            
            // Check assertion
            val (computeAssertion, tempVarAssertion) = generateExpression(conditional.assertion)
            val assertion = () :== (~"equal", (tempVarAssertion, 0)) := jumpVar
            relation.blockBuilder.addAssignments(
                computeAssertion, assertion, invert(computeAssertion)
            )
        }
        
        private def generateLoop(loop: ScopeTree.Loop): Unit = {
            // Jump to do block (from condition is checked there)
            val (doLabel, loopJump) = (relation.nextLabel(), relation.nextLabel())
            val jumpVar = relation.nextJumpVar()
            relation.nextBlock((doLabel), 0,
                jumpVar, (doLabel, loopJump))
            
            // Check from condition (must be true the first time and false in later checks)
            val (computeTest, tempVar) = generateExpression(loop.test)
            val truthVar = relation.nextTempVar()
            val truth = truthVar :== ("equal", (tempVar, 0)) := ()
            val checkTruth = () :== (~"dup", truthVar) := (jumpVar)
            
            relation.blockBuilder.addAssignments(
                computeTest, truth, checkTruth, invert(truth), invert(computeTest)
            )
            // Do
            loop.doStatements.foreach(generateStatement(_))
            // End loop or do 'loop'
            val (computeAssertion, tempVarAssertion) = generateExpression(loop.assertion)
            val (loopLabel, doJump) = (relation.nextLabel(), relation.nextLabel())
            val assertVar = relation.nextTempVar()
            val assertion = assertVar :== ("equal", (tempVarAssertion, 0)) := ()
            
            relation.blockBuilder.addAssignments(
                computeAssertion, assertion, invert(computeAssertion)
            )
            relation.nextBlock((doJump, loopLabel), assertVar,
                0, (loopLabel))
            // Loop
            loop.loopStatements.foreach(generateStatement(_))
            
            // Back to do
            relation.nextBlock((loopJump), 0,
                0, (doJump))
        }
        
        private def generateAssignment(assignment: ScopeTree.Assignment): Unit = {
            val (compute, tempVar) = generateExpression(assignment.value)
            // TODO: Arrays
            relation.blockBuilder.addAssignments(
                compute, (assignment.assignee.name :== (convert(assignment.op), tempVar) := assignment.assignee.name), invert(compute)
            )
        }
        
        private def generateSwap(swap: ScopeTree.Swap): Unit = {
            // TODO: Arrays
            relation.blockBuilder.addAssignment(
                (swap.left.name, swap.right.name) :== ("id", ()) := (swap.right.name, swap.left.name)
            )
        }

        private def generateCall(callee: Option[VariableReference], _method: Option[Method], args: Seq[Option[Variable]], invert: Boolean): Unit = {
            // All .get calls have been checked in ScopeTree and cannot fail
            val method = _method.get
            val (clazz: ScopeTree.Class, target: String) = callee match
                case None => (method.parent, "_this")
                case Some(varRef) => ((varRef.variable.get.typ match
                    case Typing.Class(typ) => typ
                    case Typing.ClassArray(typ) => typ
                    // Wellformedness ensures this case never happens
                    case _ => ???
                    ), varRef.variable.get.name.name)

            var call: Expression = s"_${clazz.name}.${method.name}"
            if invert then call = ~call
            relation.blockBuilder.addAssignment(
                ((target, "_m"), args.map(_.get)) :== (call, ()) := ((target, "_m"), args.map(_.get))
            )

        }
        
        private def convert(op: Syntax.AssignmentOperator): Expression = {
            op match
                case AssignmentOperator.ADD => "add"
                case AssignmentOperator.SUB => ~"add"
                case AssignmentOperator.XOR => "xor"
        }
        
        private def convert(op: Syntax.Operator): Expression = {
            op match
                case Operator.ADD => "add"
                case Operator.SUB => ~"add"
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
        private def generateExpression(expression: ScopeTree.Expression): (Seq[Assignment], String) = {
            val temp = relation.nextTempVar()
            expression match
                case ScopeTree.Expression.Literal(value) =>
                    val assignment = temp :== ("add", value) := 0
                    (assignment, temp)
                case ScopeTree.Expression.Reference(ref) =>
                    // TODO: Arrays
                    //val assignment = temp :== ("dup", ref.name) := ()
                    (Seq(), ref.name.name)
                // How do we represent Nil?
                case ScopeTree.Expression.Nil =>
                    val assignment = temp :== ("id", ()) := ()
                    (assignment, temp)
                case ScopeTree.Expression.Binary(left, op, right) =>
                    val (computeL, lVar) = generateExpression(left)
                    val (computeR, rVar) = generateExpression(right)
                    
                    val res: Seq[Assignment] = op match
                        case Operator.ADD | Operator.SUB | Operator.XOR =>
                            // This consumes lVar but also returns an uncompute down the line, which leads to double initialization & finalization
                            // This is fixed by AutoSSA which replaces the second occurrences with an entirely new intermediate variable, solving this problem
                            val l_temp = relation.nextTempVar()
                            
                            Seq(
                                l_temp :== ("dup", lVar) := (),
                                temp :== (convert(op), rVar) := l_temp
                            )
                        
                        case _ => temp :== (convert(op), (lVar, rVar)) := ()
                    
                    (computeL ++ computeR ++ res, temp)
        }
    }
}
