package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier

object BlockGraph {
    private object Wellformedness {
        def check(context: Class, errors: LanguageError.Collector): Unit = {
            // Check field typing
            context.fields.foreach(f =>
                checkTypeExists(f, context.program, errors))
            context.methods.foreach(check(_, errors))
        }

        def check(method: Method, errors: LanguageError.Collector): Unit = {
            // Check parameter typing
            method.parameters.foreach(p =>
                checkTypeExists(p, method.program, errors))
            method.body.foreach(s => s match
                case b: Block => check(b, errors)
                case _ => ())
        }

        def check(block: Block, errors: LanguageError.Collector): Unit = {
            // Check variable typing
            checkTypeExists(block.variable, block.program, errors)
            // Check type and name of local/delocal
            // I honestly don't understand why the syntax even requires this to be specified twice when it has to be equal anyways
            if block.varName._1 != block.varName._2 then
                errors.add(BadBlockName(block.varName._2.position))
                
            if block.varType._1 != block.varType._2 then
                errors.add(BadBlockType(block.varName._2.position))

            block.body.foreach(s => s match
                case b: Block => check(b, errors)
                case _ => ())
        }

        private def checkTypeExists(variable: Variable, program: Program, errors: LanguageError.Collector): Unit = {
            variable.typ match
                case Syntax.DataType.Class(name) if program.classes.exists(_.name == name) => errors.add(MissingType(variable))
                case Syntax.DataType.ClassArray(name) if program.classes.exists(_.name == name) => errors.add(MissingType(variable))
                case _ => ()
        }
    }
    def check(program: ClassGraph.Program): Program = {
        val graph = Program(program)
        val collector = LanguageError.Collector()
        
        graph.classes.foreach(c => Wellformedness.check(c, collector))
        collector.raiseIfNonEmpty()

        graph
    }

    // BlockGraph structure
    class Program(val program: ClassGraph.Program) {
        val classes: Seq[Class] = program.classes.valueSet().toSeq.map(c => new Class(this, c))
    }

    class Class(val parent: Program, val graphClass: ClassGraph.Class) {
        val name: Syntax.ClassIdentifier = graphClass.name

        val fields: Seq[Variable] = graphClass.fields.valueSet().toSeq.map(f => Variable(f.name, f.typ, f.position))
        val methods: Seq[Method] = graphClass.methods.valueSet().toSeq.map(m => new Method(this, m))

        val program: Program = parent
    }

    class Method(val parent: Class, val method: ClassGraph.Method) {
        val name: Syntax.MethodIdentifier = method.name

        val parameters: Seq[Variable] = method.parameters.valueSet().toSeq.map(p => Variable(p.name, p.typ, p.position))
        val body: Seq[Syntax.Statement | Block] = buildBlocks(method.syntax.body, this)

        val program: Program = parent.program
    }

    private def buildBlocks(body: Syntax.Statement, context: Method | Block): Seq[Syntax.Statement | Block] = {
        body match
            case Syntax.Statement.ObjectBlock(typ, alloc, statement, dealloc) =>
                // Since object blocks are only syntactic sugar we can get rid of them here by transforming them like a local block
                Seq(Block(context,  (Syntax.DataType.Class.apply(typ), Syntax.DataType.Class.apply(typ)),
                                    (alloc, dealloc),
                                    (Syntax.Expression.Nil, Syntax.Expression.Nil),
                                    Syntax.Statement.Block(Seq(
                                        Syntax.Statement.New(Syntax.ObjectType.Class(typ), Syntax.VariableReference.Variable(alloc)),
                                        statement,
                                        Syntax.Statement.Delete(Syntax.ObjectType.Class(typ), Syntax.VariableReference.Variable(dealloc))
                                    ))))
            case Syntax.Statement.LocalBlock(typ, alloc, init, statement, detyp, dealloc, deinit) =>
                Seq(Block(context, (typ, detyp), (alloc, dealloc), (init, deinit), statement))
            case Syntax.Statement.Block(list) =>
                list.flatMap(statement => buildBlocks(statement, context))
            // This doesn't really work because it 'swallows' the conditional/loop statement
            // At this point it might become sensible to transform all statements into a better representation
            case Syntax.Statement.Conditional(test, thenStatement, elseStatement, assertion) =>
                buildBlocks(thenStatement, context) ++ buildBlocks(elseStatement, context)
            case Syntax.Statement.Loop(test, doStatement, loopStatement, assertion) =>
                buildBlocks(doStatement, context) ++ buildBlocks(loopStatement, context)
            case _ => Seq(body)
    }

    class Block(val parent: Method | Block,
                val varType: (Syntax.DataType, Syntax.DataType),
                val varName: (Syntax.VariableIdentifier, Syntax.VariableIdentifier),
                val varInit: (Syntax.Expression, Syntax.Expression),
                val statement: Syntax.Statement) {
        val body: Seq[Syntax.Statement | Block] = buildBlocks(statement, this)
        val variable: Variable = Variable(varName._1, varType._1, varName._1.position)

        val program: Program = parent match
            case m: Method => m.program
            case b: Block => b.program
    }

    case class Variable(val name: Syntax.VariableIdentifier, val typ: Syntax.DataType, val definition: SourcePosition)

    // Type/block errors
    case class MissingType(variable: Variable) extends RooplError(Error, s"specified type ${variable.typ.toString()} does not exist.", variable.definition)
    case class BadBlockType(pos: SourcePosition) extends RooplError(Error, s"type of delocal must be equal to type of local.", pos)
    case class BadBlockName(pos: SourcePosition) extends RooplError(Error, s"name of delocal must be equal to name of local.", pos)
}
