package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.{Assignment, Block, Entry, Exit, Expression, Identifier, Program, Relation}
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.plugin.{Arithmetic, Basic, Information}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import de.thm.mni.hybridcomputing.util.reversibility.Direction.{BACKWARDS, FORWARDS}

import java.nio.file.Paths
import scala.collection.mutable

object MyMain {
    
    def tuple(values: Value*): Value = {
        if (values.length == 1) {
            values.head
        } else {
            Value.Pair(values.head, tuple(values.tail *))
        }
    }
    
    class encode(program: Program) {
        val buffer = mutable.Map[String, Int]()
        
        buffer.put("begin", -10)
        buffer.put("end", -20)
        
        var counter = 0
        
        def encode(value: Value): Value = value match {
            case Basic.Unit => Basic.Unit
        }
        
        def encode(name: String): Basic.Int = {
            
            
            Basic.Int(buffer.getOrElseUpdate(name, {
                counter += 1
                counter
            }))
        }
        
        def encode(identifier: Identifier): Basic.Int = encode(identifier.name)
        
        def encode[A](seq: List[A], encoder: A => Value): Value = seq match {
            case ::(head, next) => Value.Pair(Basic.Int(1), Value.Pair(encoder(head), encode(next, encoder)))
            case Nil => Value.Pair(Basic.Int(0), Value.Pair(Basic.Unit, Basic.Unit))
        }
        
        def encode(exp: Expression): Value = exp match {
            case Expression.Literal(value) => Value.Pair(Basic.Int(0), Basic.Int(value))
            case Expression.Unit() => Value.Pair(Basic.Int(0), Basic.Unit)
            case Expression.Variable(name) => Value.Pair(Basic.Int(1), encode(name))
            case Expression.Pair(a, b) => Value.Pair(Basic.Int(2), Value.Pair(encode(a), encode(b)))
            case Expression.Invert(a) => Value.Pair(Basic.Int(3), encode(a))
        }
        
        def encode(assignment: Assignment): Value =
            tuple(
                encode(assignment.target),
                encode(assignment.relation),
                encode(assignment.instance_argument),
                encode(assignment.source),
            )
        
        def encode(entry: Entry): Value = Value.Pair(encode(entry.initialized), encode(entry.labels.toList, this.encode))
        def encode(exit: Exit): Value = Value.Pair(encode(exit.labels.toList, this.encode), encode(exit.argument))
        def encode(block: Block): Value = tuple(
            encode(block.entry),
            encode(block.assignments.toList, this.encode),
            encode(block.exit)
        )
        
        def encode(procedure: Relation): Value = tuple(
            encode(procedure.name),
            encode(procedure.parameter),
            encode(procedure.blocks.toList, this.encode)
        )
        
        lazy val starting_store: Value = {
            
            program.language.builtins.map(b => (encode(b.value.name), b.value))
              .sortBy(-_._1.value)
              .foldRight(tuple(Basic.Int(-1), Basic.Unit, Basic.Unit))((builtin, tail) => {
                  tuple(
                      builtin._1,
                      Value.Pair(Value.Pair(Basic.Int(0), builtin._2), Basic.Int(0)),
                      tail
                  )
              })
        }
        
        lazy val encoded = encode(program.definitions.toList, this.encode)
    }
    
    def main(args: Array[String]): Unit = {
        try {
            val language = Language(Seq(Basic, Arithmetic, Information), Language.Canon.semantics)
            
            val prog = Parsing(language).parse(lex(SourceFile.fromFile(Paths.get("programs/examples/selfinterpreter.hssa"))))
            val fibpair_prog = Parsing(language).parse(lex(SourceFile.fromFile(Paths.get("programs/examples/fibpair.hssa"))))
            
            val encoded = new encode(fibpair_prog)
            
            Interpretation(language).interpret(
                prog,
                "main",
                tuple(
                    encoded.starting_store,
                    encoded.encoded,
                    encoded.encode("fibpair.test"),
                    encoded.encode(Basic.Unit),
                ),
                encoded.encode(Basic.Unit)
            )
            
        } catch {
            case e: LanguageError.AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(e)
                    println()
                })
        }
    }
}
