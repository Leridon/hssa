package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.{Assignment, Block, Entry, Exit, Expression, Identifier, Program, Relation}
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.plugin.Basic

import scala.collection.mutable

class SelfInterpretationEncoder(program: Program) {
    val buffer = mutable.Map[String, Int]()
    
    buffer.put("begin", -10)
    buffer.put("end", -20)
    
    var counter = 0
    
    def encode(value: Value): Value = value
    
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
        case Expression.Wildcard() => Value.Pair(Basic.Int(4), Basic.Unit)
        case Expression.Duplicate(a) => Value.Pair(Basic.Int(5), encode(a))
    }
    
    def encode(assignment: Assignment): Value =
        SelfInterpretationEncoder.tuple(
            encode(assignment.output),
            encode(assignment.callee),
            encode(assignment.parameter),
            encode(assignment.input),
        )
    
    def encode(entry: Entry): Value = Value.Pair(encode(entry.output), encode(entry.labels.toList, this.encode))
    def encode(exit: Exit): Value = Value.Pair(encode(exit.labels.toList, this.encode), encode(exit.input))
    def encode(block: Block): Value = SelfInterpretationEncoder.tuple(
        encode(block.entry),
        encode(block.assignments.toList, this.encode),
        encode(block.exit)
    )
    
    def encode(procedure: Relation): Value = SelfInterpretationEncoder.tuple(
        encode(procedure.name),
        encode(procedure.parameter),
        encode(procedure.blocks.toList, this.encode)
    )
    
    lazy val starting_store: Value = {
        
        program.language.builtins.map(b => (encode(b.value.name), b.value))
          .sortBy(-_._1.value)
          .foldRight(SelfInterpretationEncoder.tuple(Basic.Int(-1), Basic.Unit, Basic.Unit))((builtin, tail) => {
              SelfInterpretationEncoder.tuple(
                  builtin._1,
                  Value.Pair(Value.Pair(Basic.Int(0), builtin._2), Basic.Int(0)),
                  tail
              )
          })
    }
    
    lazy val encoded = encode(program.definitions.toList, this.encode)
}

object SelfInterpretationEncoder {
    def tuple(values: Value*): Value = {
        if (values.length == 1) {
            values.head
        } else {
            Value.Pair(values.head, tuple(values.tail *))
        }
    }
}