package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.Types
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.Errors.ReversibilityViolation
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.util.reversibility.Direction.{BACKWARDS, FORWARDS}

object Arrays extends Plugin {
    
    import PrimitiveCreationHelpers.*
    
    case class Array(elements: scala.Array[Value]) extends Value
    
    object ArrayType extends Types.Type
    
    def array_parameter(f: scala.Array[Value] => InstantiatedFunction): Function = {
        case Array(elements) => f(elements)
    }
    
    given RuntimeCheckable[Array] with
        override def check(value: Value): Array = value match {
            case a@Array(elements) => a
            case _ => ReversibilityViolation("").raise()
        }
    
    override def requirements: Seq[Plugin] = super.requirements
    override def builtins: Seq[Plugin.Builtin] = Seq(
        builtin("array.new", checkedPar((size: Int) => {
            case FORWARDS => checkedUnit(Array(new scala.Array[Value](size).map(_ => Value.Unit)))
            case BACKWARDS => consumeIf(checked((a: Array) => size == a.elements.length && a.elements.forall(e => e == Value.Unit)))
        }), Types.ParameterizedRelation(Types.Int, Types.Unit, ArrayType)),
        builtin("array.readwrite", checkedPar((index: Int) => _ => checked((array: Array, write_value: Value) => {
            val read_value = array.elements(index)
            val new_array = Array(array.elements.updated(index, write_value))
            
            Value.Pair(
                new_array, read_value
            )
        })
        ), Types.ParameterizedRelation(Types.Int, Types.Pair(ArrayType, new Types.MetaVariable), Types.Pair(ArrayType, new Types.MetaVariable))),
        builtin("array.length", checkedPar((array: Array) => {
            case FORWARDS => produce(Value.Int(array.elements.length))
            case BACKWARDS => consume(Value.Int(array.elements.length))
        }), Types.ParameterizedRelation(ArrayType, Types.Unit, Types.Int))
    )
}
