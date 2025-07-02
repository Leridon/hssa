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
import de.thm.mni.hybridcomputing.roopl.wellformedness.Translatable
import de.thm.mni.hybridcomputing.roopl.wellformedness.Translatable.TypedVariable
import de.thm.mni.hybridcomputing.roopl.wellformedness.Translatable.Types

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
            clazz.methods.map(method => Generator.generateRelation(clazz.name, method))
                .appended(Generator.generateVtable(clazz))
                .appended(Generator.generateConstructor(clazz))
        })
            .appended(main)
            .appended(Generator.generateMemoryRead())

        hssa.Syntax.Program(relations, language)
    }

    class Relation(val name: String, val parameter: Seq[Variable]) {
        // _this refers to the object on which a method is called
        // All methods have a callee except main, for which a special object is created
        val builder: RelationBuilder = RelationBuilder(name, appendVarsIfNotEmpty(Generator._this, parameter))
        val locals: mutable.Stack[Variable] = mutable.Stack[Variable]()
        val tempVars = UniqueNameGenerator(".")
        // Keeps the currently unfinished block, the first block always entries with "begin"
        var blockBuilder = BlockBuilder(builder, (Generator.mmem, 0) :=<- "begin")
        
        def nextTempVar(): String = tempVars.next("_t")
        def nextAddrVar(): String = tempVars.next("_addr")
        def nextJumpVar(): String = tempVars.next("_j")
        def nextLabel(): String = builder.label_generator.next("L")
        // Use to end the current block and start a new one
        def nextBlock(exitLabels: Seq[String], exitJump: Expression, entryJump: Expression, entryLabels: Seq[String]): Unit = {
            val exit = ->(exitLabels) := (((Generator.mmem, Generator.thisRef), locals.toSeq), exitJump)
            val entry = (((Generator.mmem, Generator.thisRef), locals.toSeq), entryJump) :=<- entryLabels
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

    private def appendVarsIfNotEmpty(expression: Expression, seq: Seq[Variable]): Expression = {
        seq match
            case Seq() => expression
            case s => (expression, s)
    }
    
    private object Generator {
        var relation: Relation = null
        val mmem: String = "_m"
        val _this: String ="_this"
        val thisRef = "_this_ref"


        def generateMain(program: ScopeTree.Program): hssa.Syntax.Relation = {
            // Setup managed memory, initialize object of main class and call roopl main
            this.relation = Relation("main", Seq())
            relation.blockBuilder = BlockBuilder(relation.builder, ((), 0) :=<- ("begin"))
            // The helper generates a _this parameter for all relations. hssa main (as opposed to roopl main) can't have a this context
            relation.builder.parameter = ()

            val setup = Seq(
                    mmem :== ("mmem.new", ()) := (),
                    // Create _this for main
                    (mmem, "_main") :== ("mmem.allocate", 1) := mmem,
                    (mmem, "_mainObj") :== (constructor(program.mainClass), ()) := mmem,
                    (mmem, 0) :== ("mmem.readwrite", "_main") := (mmem, "_mainObj"),
            )

            relation.blockBuilder.adds(
                (setup) ++
                (mmem :== (s"_${program.mainClass.name}.${program.mainMethod.name}", ("_main")) := mmem),
                () :== (~"dup", 1) := "_main")
            relation.blockBuilder.finish(->("end") := ((mmem), 0))

            relation.builder.compile()
        }

        def generateMemoryRead(): hssa.Syntax.Relation = {
            val addr = "_addr"
            val relation = RelationBuilder("mmem.read", addr)
            val block = BlockBuilder(relation, (mmem, 0) :=<- "begin")

            val value = "_readvalue"
            val res = "_readcopy"
            block.adds(
                (mmem, value) :== ("mmem.readwrite", addr) := (mmem, ()),
                res :== ("dup", value) := (),
                (mmem, ()) :== ("mmem.readwrite", addr) := (mmem, value)
            )
            block.finish(->("end") := ((mmem, res), 0))

            relation.compile()
        }
        
        def generateRelation(className: Syntax.ClassIdentifier, method: Method): hssa.Syntax.Relation = {
            relation = Relation(s"_$className.${method.name}", method.parameters)
            // Make all fields accessible
            /* 
            
                class Cat
                    int x
                    Cat friend
                    method blub()
                        x + friend
            
                m, ref := mmem.read _this := m
                x := add ref := 0
                friend := add ref := 1
             */
            
            val initFields = method.parent.fields.zipWithIndex.map((field, index) => 
                field :== ("add", thisRef) := index + 1
            ).prepended((mmem, thisRef) :== ("mmem.read", _this) := mmem)

            method.parent.fields.foreach(relation.locals.push)
            relation.blockBuilder.adds(initFields)

            method.translatableBody.foreach(generateStatement)

            relation.blockBuilder.adds(invert(initFields))
            method.parent.fields.foreach(_ => relation.locals.pop())
            
            relation.blockBuilder.finish(->("end") := (mmem, 0))
            relation.builder.compile()
        }

        def generateVtable(clazz: ScopeTree.Class): hssa.Syntax.Relation = {
            val builder = RelationBuilder(vtable(clazz), ("_method"))
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

        def generateConstructor(clazz: ScopeTree.Class): hssa.Syntax.Relation = {
            val builder = RelationBuilder(constructor(clazz), ())
            val block = BlockBuilder(builder, (mmem, 0) :=<- "begin")
            val anonObj = "_anon"
            val table = "_vtable"

            // Allocate #fields + 1 slot (vtable) in memory for the new object
            block.adds(
                (mmem, anonObj) :== ("mmem.allocate", clazz.fields.size + 1) := mmem,
                table :== ("dup", vtable(clazz)) := (),
                (mmem, 0) :== ("mmem.readwrite", anonObj) := (mmem, table)
            )

            block.finish(->("end") := ((mmem, anonObj), 0))
            builder.compile()
        }

        private def vtable(clazz: ScopeTree.Class): String = {
            s"_${clazz.name}.vtable"
        }

        private def constructor(clazz: ScopeTree.Class): String = {
            s"_${clazz.name}.new"
        }
        
        private def generateStatement(statement: Translatable.StatementNode): Unit = {
            statement match
                case conditional: Translatable.Conditional => generateConditional(conditional)
                case loop: Translatable.Loop => generateLoop(loop)
                case assignment: Translatable.Assignment => generateAssignment(assignment)
                case swap: Translatable.Swap => generateSwap(swap)
                case Translatable.New(typ, name) => generateNew(typ, name)
                case Translatable.Delete(typ, name) => generateDelete(typ, name)
                case Translatable.Copy(typ, from, to) => generateCopy(typ, from, to)
                case Translatable.Uncopy(typ, from, to) => generateUncopy(typ, from, to)
                case Translatable.Call(callee, method, args) => generateCall(callee, method, args, false)
                case Translatable.Uncall(callee, method, args) => generateCall(callee, method, args, true)
                case block: ScopeTree.Block => generateBlock(block)
                case Translatable.BadStatement() => ??? // TODO: throw proper exception
        }
        
        // Compute variable, do body, uncompute variable and garbage
        private def generateBlock(block: ScopeTree.Block): Unit = {
            // Add variable to locals, so it can be propagated between sub-blocks
            relation.locals.push(block.variable)
            
            // tmp must be an int value or a reference (pointer) to an object or nil
            val (localCompute, localTmp) = generateExpression(block.translatableCompute)
            
            relation.blockBuilder.adds(
                localCompute,
                "_tmp" :== ("dup", localTmp) := (),
                (mmem, block.variable) :== ("mmem.allocate", 1) := mmem,
                (mmem, 0) :== ("mmem.readwrite", block.variable) := (mmem, "_tmp"),
                invert(localCompute) // We need to clean up any potential temporaries left behind
            )
            
            block.translatableBody.foreach(generateStatement)
            
            val (delocalCompute, delocalTmp) = generateExpression(block.translatableUncompute)
            
            relation.blockBuilder.adds(
                delocalCompute,
                (mmem, "_tmp") :== ("mmem.readwrite", block.variable) := (mmem, 0),
                () :== (~"dup", delocalTmp) := "_tmp",
                mmem :== (~"mmem.allocate", 1) := (mmem, block.variable),
                invert(delocalCompute)
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

            val (computeAddress, address) = assignment.assignee.index match
                case None => (Seq(), assignment.assignee.variable.name.name)
                case Some(index) => (indexedAddress(assignment.assignee.variable, index))
            val value = "_assignee_val"

            relation.blockBuilder.adds(
                compute,
                computeAddress,
                (mmem, value) :== ("mmem.readwrite", address) := (mmem, ()),
                value :== (convert(assignment.op), tempVar) := value,
                (mmem, ()) :== ("mmem.readwrite", address) := (mmem, value),
                invert(computeAddress),
                invert(compute)
            )
        }
        
        private def generateSwap(swap: Translatable.Swap): Unit = {
            /*
                int f, g

                int[8] arr

                f <=> g
                arr[5] <=> f

                m, f_val := mmem.readwrite f := m, ()
                m, g_val := mmem.readwrite g := m, f_val
                m, ()    := mmem.readwrite f := m, g_val

                tmp         := add arr            := 5
                m, arr5_val := mmem.readwrite tmp := m, ()
                m, f_val    := mmem.readwrite f   := m, arr5_val
                m, ()       := mmem.readwrite tmp := m, f_val

            */
            val (computeLeft, left) = referenceAddress(swap.left)
            val (computeRight, right) = referenceAddress(swap.right)
            val leftVal = "_leftVal"
            val rightVal = "_rightVal"

            relation.blockBuilder.adds(
                computeLeft,
                computeRight,
                (mmem, leftVal) :== ("mmem.readwrite", left) := (mmem, ()),
                (mmem, rightVal) :== ("mmem.readwrite", right) := (mmem, leftVal),
                (mmem, ()) :== ("mmem.readwrite", left) := (mmem, rightVal),
                invert(computeRight),
                invert(computeLeft)
            )
        }

        private def generateCall(callee: Option[Translatable.VariableReference], method: Method, args: Seq[Variable], inverted: Boolean): Unit = {
            /* 
                Animal a
                new Cat a
                int i
                Animal friend
                new Animal friend

                a::meow(i, friend)

                <init variables>
                m, ref := mmem.read a := m
                m, vtable := mmem.read ref := m
                // meow is the second method
                method := vtable 3 := ()
                m := method (a, i, friend) := m

                (_this)::meow(i, friend)
                <init variables>
                m, ref := mmem.read _this := m
             */
            val (computeCalleeAddr, calleeAddr) = callee match
                case None => (Seq(), _this)
                case Some(varRef) => referenceAddress(varRef)
            val ref = "_calleeRef"
            val vtable = "_calleeVtable"
            val calledMethod = "_calleeMethod"
            
            val lookupVtable = Seq(
                (mmem, ref) :== ("mmem.read", calleeAddr) := mmem,
                (mmem, vtable) :== ("mmem.read", ref) := mmem,
                calledMethod :== (vtable, method.parent.methods.indexOf(method)) := ()
            )
            relation.blockBuilder.adds(
                lookupVtable,
                mmem :== (if inverted then ~calledMethod else calledMethod, appendVarsIfNotEmpty(calleeAddr, args)) := mmem,
                invert(lookupVtable)
            )
        }

        private def generateNew(typ: Types.ArrayType | Types.Class, name: Translatable.VariableReference): Unit = {
            relation.blockBuilder.adds(generateNewCode(typ, name))
        }

        private def generateDelete(typ: Types.ArrayType | Types.Class, name: Translatable.VariableReference): Unit = {
            relation.blockBuilder.adds(invert(generateNewCode(typ, name)))
        }

        private def generateNewCode(typ: Types.ArrayType | Types.Class, name: Translatable.VariableReference): Seq[Assignment] = {
            var assignments: Seq[Assignment] = Seq()

            val (computeAddr, addr) = referenceAddress(name)
            val anon = "_newRef"

            typ match
                case typ: Types.ArrayType =>
                    val (compute, tmpVar) = generateExpression(typ.size)
                    assignments = compute ++
                        // Ensure array index is >= 0
                        (1 :== ("greaterequal", (tmpVar, 0)) := ()) ++
                        ((mmem, anon) :== ("mmem.allocate", tmpVar) := mmem) ++
                        invert(compute)
                case Types.Class(typ) =>
                    assignments = Seq(
                        (mmem, anon) :== (constructor(typ), ()) := mmem
                    )
            computeAddr ++
            assignments ++ (
                (mmem, 0) :== ("mmem.readwrite", addr) := (mmem, anon)
            ) ++ invert(computeAddr)
        }

        private def generateCopy(typ: Types.ArrayType | Types.Class, from: Translatable.VariableReference, to: Translatable.VariableReference): Unit = {
            relation.blockBuilder.adds(
                generateCopyCode(from, to)
            )
        }

        private def generateUncopy(typ: Types.ArrayType | Types.Class, from: Translatable.VariableReference, to: Translatable.VariableReference): Unit = {
            relation.blockBuilder.adds(
                invert(generateCopyCode(from, to))
            )
        }

        private def generateCopyCode(from: Translatable.VariableReference, to: Translatable.VariableReference): Seq[Assignment] = {
            /* 
                Cat a -> 1
                Cat b -> 2

                new Cat a -> 3-5
                copy Cat a b 

                <new>
                m, addr_a   := mmem.readwrite a     := m, ()
                addr_copy := dup addr_a := ()
                m, () := mmem.readwrite a := m, addr_a
                m, 0 := mmem.readwrite b := m, addr_copy

                // alt with read
                m, copy := mmem.read a := m
                m, 0 := mmem.readwrite b := m, copy


            
            */
            val (computeFromAddr, fromAddr) = referenceAddress(from)
            val (computeToAddr, toAddr) = referenceAddress(to)
            val refCopy = "_ref_copy"

                computeFromAddr ++
                computeToAddr ++
                ((mmem, refCopy) :== ("mmem.read", fromAddr) := mmem) ++
                ((mmem, 0) :== ("mmem.readwrite", toAddr) := (mmem, refCopy)) ++
                invert(computeToAddr) ++
                invert(computeFromAddr)
        }

        private def referenceAddress(ref: Translatable.VariableReference): (Seq[Assignment], String) = {
           ref.index match
                case None => (Seq(), ref.variable.name.name)
                case Some(index) => indexedAddress(ref.variable, index)
        }

        private def indexedAddress(array: TypedVariable, index: Translatable.Expression): (Seq[Assignment], String) = {
            val indexVal = "_index"
            val address = relation.nextAddrVar()
            val (compute, tempVar) = generateExpression(index)
            (compute ++
                ((mmem, address) :== ("mmem.read", array.name) := (mmem)) ++
                (address :== ("add", tempVar) := address) ++
                invert(compute)
            , address)
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
                    val (computeAddr, addr) = referenceAddress(ref)
                    val assignment = (mmem, temp) :== ("mmem.read", addr) := mmem
                    (computeAddr ++ assignment ++ invert(computeAddr), temp)
                case Translatable.Expression.Nil =>
                    // Nullpointer
                    val assignment = temp :== ("id", ()) := 0
                    (assignment, temp)
                case Translatable.Expression.Binary(left, op, right) =>
                    val (computeL, lVar) = generateExpression(left)
                    val (computeR, rVar) = generateExpression(right)
                    
                    val res: Seq[Assignment] = op match
                        case Operator.ADD | Operator.SUB | Operator.XOR =>
                            val lTemp = relation.nextTempVar()
                            
                            Seq(
                                lTemp :== ("dup", lVar) := (),
                                temp :== (convert(op), rVar) := lTemp
                            )
                        
                        case _ => temp :== (convert(op), (lVar, rVar)) := ()
                    
                    (computeL ++ computeR ++ res, temp)
        }
    }
}
