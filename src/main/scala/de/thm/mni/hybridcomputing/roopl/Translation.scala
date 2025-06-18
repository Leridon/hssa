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
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Variable
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import scala.collection.mutable
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Method
import de.thm.mni.hybridcomputing.hssa.Syntax.Statement
import de.thm.mni.hybridcomputing.hssa.util.BlockBuilder
import scala.runtime.stdLibPatches.language.experimental.genericNumberLiterals
import de.thm.mni.hybridcomputing.roopl.wellformedness.Typing
import de.thm.mni.hybridcomputing.roopl.wellformedness.Translatable

object Translation {
    given Expressionable[Variable] with
        def toExpression(v: Variable): Expression = v.name.name
    
    given Expressionable[Syntax.VariableIdentifier] with
        def toExpression(v: Syntax.VariableIdentifier): Expression = v.name

    given Expressionable[Syntax.ClassIdentifier] with
        def toExpression(v: Syntax.ClassIdentifier): Expression = v.name

    given Expressionable[Seq[Variable]] with
        def toExpression(s: Seq[Variable]): Expression = s match
            case Seq() => Expression.Unit()
            case _ => s.map[Expression](identity).reduce((a, b) => (a, b))

    def translateRooplToHssa(program: ScopeTree.Program, language: Language): hssa.Syntax.Program = {
        val main: hssa.Syntax.Relation = Generator.generateMain(program)

        // Generate program
        val relations: Seq[hssa.Syntax.Relation] = program.classes.flatMap(clazz => {
            clazz.methods.map(method => {
                val relation = Relation(s"_${clazz.name}.${method.name}", method.parameters)
                Generator.generateRelation(relation, method)
                relation.builder.compile()
            }).appended(Generator.generateVtable(clazz))
        }).appended(main)

        hssa.Syntax.Program(relations, language)
    }

    class Relation(val name: String, val parameter: Seq[Variable]) {
        // _this refers to the object on which a method is called
        // All methods have a callee except main, for which a special object is created
        val builder: RelationBuilder = RelationBuilder(name, ("_this"))
        val locals: mutable.Stack[Variable] = mutable.Stack[Variable]()
        val tempVars = UniqueNameGenerator(".")
        // Keeps the currently unfinished block, the first block always entries with "begin"
        var blockBuilder = BlockBuilder(builder, (Generator.hssaParameters(parameter), 0) :=<- "begin")
        
        def nextTempVar(): String = tempVars.next("_t")
        def nextJumpVar(): String = tempVars.next("_j")
        def nextObjVar(): String = tempVars.next("_new")
        def nextLabel(): String = builder.label_generator.next("L")
        // Use to end the current block and start a new one
        def nextBlock(exitLabels: Seq[String], exitJump: Expression, entryJump: Expression, entryLabels: Seq[String]): Unit = {
            val exit = ->(exitLabels) := (locals.toSeq, exitJump)
            val entry = (locals.toSeq, entryJump) :=<- entryLabels
            blockBuilder.finish(exit)
            blockBuilder = BlockBuilder(builder, entry)
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

        def generateMain(program: ScopeTree.Program): hssa.Syntax.Relation = {
            // Setup managed memory, initialize object of main class and call roopl main
            this.relation = Relation("main", Seq())
            relation.blockBuilder = BlockBuilder(relation.builder, ((), 0) :=<- ("begin"))
            relation.builder.parameter = ()

            val (gen, mainObj) = generateObject(program.mainClass)

            val setup = Seq(
                    gen,
                    "m" :== ("mmem.new", ()) := (),
                    // Create _this for main
                    ("m", "_main") :== ("mmem.allocate", 1) := "m",
                    ("m", ()) :== ("mmem.readwrite", "_main") := ("m", mainObj)
            )

            relation.blockBuilder.adds(
                (setup) ++
                (("m") :== (s"_${program.mainClass.name}.${program.mainMethod.name}", ("_main")) := ("m")) ++
                (invert(setup)))
            relation.blockBuilder.finish(->("end") := ((), 0))

            relation.builder.compile()
        }
        
        def generateRelation(relation: Relation, method: Method): Unit = {
            this.relation = relation
            method.translatableBody.foreach(generateStatement)
            
            relation.blockBuilder.finish(->("end") := (hssaParameters(relation.parameter), 0))
        }

        def generateVtable(clazz: ScopeTree.Class): hssa.Syntax.Relation = {
            val builder = RelationBuilder(s"_${clazz.name}.vtable", ("_method"))
            var block = BlockBuilder(builder, ((), 0) :=<- ("begin"))
            block.add(
                "i" :== ("dup", "_method") := ()
            )

            val methods: Seq[(String, Assignment, String)] = clazz.methods.map[(String, Assignment, String)](method => {
                (relation.nextLabel(), "res" :== ("dup", s"_${clazz.name}.${method.name}") := (), relation.nextLabel())
            })

            block.finish(
                ->(methods.map(_._1)) := ((), "i")
            )

            methods.foreach((entry, code, exit) =>
                block = BlockBuilder(builder, ((), 0) :=<- entry)
                block.add(code)
                block.finish(->(exit) := ("res", 0))
            )

            block = BlockBuilder(builder, ("res", "i") :=<- methods.map(_._3))
            block.add(
                () :== (~"dup", "_method") := "i"
            )
            block.finish(->("end") := ("res", 0))

            builder.compile()
        }

        def hssaParameters(parameters: Seq[Variable]): Expression = {
            parameters match
                case Seq() => ("_m")
                case _ => (("_m"), parameters)
        }
        
        private def generateStatement(statement: Translatable.StatementNode): Unit = {
            statement match
                case conditional: Translatable.Conditional => generateConditional(conditional)
                case loop: Translatable.Loop => generateLoop(loop)
                case assignment: Translatable.Assignment => generateAssignment(assignment)
                case swap: Translatable.Swap => generateSwap(swap)
                case Translatable.New(typ, name) => generateNew(typ, name)
                case Translatable.Delete(typ, name) => generateDelete(typ, name)
                case Translatable.Copy(typ, from, to) => ???
                case Translatable.Uncopy(typ, from, to) => ???
                case Translatable.Call(callee, method, args) => generateCall(callee, method, args, false)
                case Translatable.Uncall(callee, method, args) => generateCall(callee, method, args, false)
                case block: ScopeTree.Block => generateBlock(block)
        }
        
        // Compute variable, do body, uncompute variable and garbage
        private def generateBlock(block: ScopeTree.Block): Unit = {
            // Add variable to locals, so it can be propagated between sub-blocks
            relation.locals.push(block.variable)
            
            val (local_compute, local_temp_var) = generateExpression(block.translatableCompute)
            
            relation.blockBuilder.adds(
                local_compute,
                block.variable :== ("dup", local_temp_var) := (),
                invert(local_compute) // We need to clean up any potential temporaries left behind
            )
            
            block.translatableBody.foreach(generateStatement)
            
            val (delocal_compute, delocal_temp_var) = generateExpression(block.translatableUncompute)
            
            relation.blockBuilder.adds(
                delocal_compute,
                () :== (~"dup", delocal_temp_var) := block.variable,
                invert(delocal_compute)
            )
            // Pop variable from locals
            relation.locals.pop()
        }
        
        private def generateConditional(conditional: Translatable.Conditional): Unit = {
            // Check condition
            val (computeTest, tempVar) = generateExpression(conditional.test)
            val truthVar = relation.nextTempVar()
            val truth = truthVar :== ("equal", (tempVar, 0)) := ()
            val (thenLabel, elseLabel) = (relation.nextLabel(), relation.nextLabel())
            
            relation.blockBuilder.adds(
                computeTest, truth, invert(computeTest)
            )
            // Jump to then or else
            relation.nextBlock((thenLabel, elseLabel), truthVar,
                0, thenLabel)
            // Then
            conditional.thenStatements.foreach(generateStatement)
            
            // Else
            val (thenJump, elseJump) = (relation.nextLabel(), relation.nextLabel())
            relation.nextBlock((thenJump), 0,
                0, elseLabel)
            conditional.elseStatements.foreach(generateStatement)
            
            val jumpVar = relation.nextJumpVar()
            relation.nextBlock((elseJump), 0,
                jumpVar, (thenJump, elseJump))
            
            // Check assertion
            val (computeAssertion, tempVarAssertion) = generateExpression(conditional.assertion)
            val assertion = () :== (~"equal", (tempVarAssertion, 0)) := jumpVar
            relation.blockBuilder.adds(
                computeAssertion, assertion, invert(computeAssertion)
            )
        }
        
        private def generateLoop(loop: Translatable.Loop): Unit = {
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
            
            relation.blockBuilder.adds(
                computeTest, truth, checkTruth, invert(truth), invert(computeTest)
            )
            // Do
            loop.doStatements.foreach(generateStatement)
            // End loop or do 'loop'
            val (computeAssertion, tempVarAssertion) = generateExpression(loop.assertion)
            val (loopLabel, doJump) = (relation.nextLabel(), relation.nextLabel())
            val assertVar = relation.nextTempVar()
            val assertion = assertVar :== ("equal", (tempVarAssertion, 0)) := ()
            
            relation.blockBuilder.adds(
                computeAssertion, assertion, invert(computeAssertion)
            )
            relation.nextBlock((doJump, loopLabel), assertVar,
                0, (loopLabel))
            // Loop
            loop.loopStatements.foreach(generateStatement)
            
            // Back to do
            relation.nextBlock((loopJump), 0,
                0, (doJump))
        }
        
        private def generateAssignment(assignment: Translatable.Assignment): Unit = {
            val (compute, tempVar) = generateExpression(assignment.value)
            // TODO: Arrays
            relation.blockBuilder.adds(
                compute, (assignment.assignee.name :== (convert(assignment.op), tempVar) := assignment.assignee.name), invert(compute)
            )
        }
        
        private def generateSwap(swap: Translatable.Swap): Unit = {
            // TODO: Arrays
            relation.blockBuilder.add(
                (swap.left.name, swap.right.name) :== ("id", ()) := (swap.right.name, swap.left.name)
            )
        }

        private def generateCall(callee: Option[Translatable.VariableReference], method: Method, args: Seq[Variable], invert: Boolean): Unit = {
            val (clazz: ScopeTree.Class, target: String) = callee match
                case None => (method.parent, "_this")
                case Some(varRef) => ((varRef.variable.typ match
                    case Typing.Class(typ) => typ
                    case Typing.ClassArray(typ) => typ
                    // Wellformedness ensures this case never happens
                    case _ => ???
                    ), varRef.variable.name.name)

            var call: Expression = s"_${clazz.name}.${method.name}"
            if invert then call = ~call
            relation.blockBuilder.add(
                ((target, "_m"), args) :== (call, ()) := ((target, "_m"), args)
            )
        }

        private def generateNew(typ: Typing.ArrayType | Typing.Class, name: Translatable.VariableReference): Unit = {
            typ match
                case Typing.IntegerArray => ???
                case Typing.ClassArray(typ) => ???
                case Typing.Class(typ) => 
                    val (gen, objVar) = generateObject(typ)
                    name.variable.owner match
                        case b: ScopeTree.Block => relation.blockBuilder.adds(
                            gen,
                            (name.name, ()) :== ("id", ()) := (objVar, name.name)
                        )
        }

        private def generateDelete(typ: Typing.ArrayType | Typing.Class, name: Translatable.VariableReference): Unit = {
            typ match
                case Typing.IntegerArray => ???
                case Typing.ClassArray(typ) => ???
                case Typing.Class(typ) => 
                    val (gen, objVar) = generateObject(typ)
                    name.variable.owner match
                        case b: ScopeTree.Block => relation.blockBuilder.adds(
                            gen,
                            (name.name, ()) :== (~"id", ()) := (objVar, name.name)
                        )
        }

        private def generateObject(typ: ScopeTree.Class): (Assignment, Expression) = {
            // Generate a tuple where the first item is the class name (for vtable lookup) and each following item are the fields in order
            val name = s"_${typ.name}.vtable"
            val objValue: Expression = typ.fields match
                case Seq() => "_vtable"
                case _ => ("_vtable", zeroClearedTuple(typ.fields))

            ("_vtable" :== ("dup", name) := (), objValue)
        }

        private def zeroClearedTuple(values: Seq[Any]): Expression = {
            values.map[Expression](_ => ()).reduce((a,b) => (a,b))
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
        private def generateExpression(expression: Translatable.Expression): (Seq[Assignment], String) = {
            val temp = relation.nextTempVar()
            expression match
                case Translatable.Expression.Literal(value) =>
                    val assignment = temp :== ("add", value) := 0
                    (assignment, temp)
                case Translatable.Expression.Reference(ref) =>
                    // TODO: Arrays
                    //val assignment = temp :== ("dup", ref.name) := ()
                    (Seq(), ref.name.name)
                // How do we represent Nil?
                case Translatable.Expression.Nil =>
                    val assignment = temp :== ("id", ()) := ()
                    (assignment, temp)
                case Translatable.Expression.Binary(left, op, right) =>
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
