package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.Types
import de.thm.mni.hybridcomputing.hssa.interpretation.Value

object Arrays extends Plugin {
    
    import BuiltinCreationHelpers.*
    
    case class Array(elements: scala.Array[Value]) extends Value
    
    object ArrayType extends Types.Type
    
    override def requirements: Seq[Plugin] = super.requirements
    override def builtins: Seq[Plugin.Builtin] = Seq(
        
        FunctionPair(
            int_parameter(size => unit_input({
                Array(new scala.Array[Value](size).map(_ => Basic.Unit))
            })),
            
            int_parameter(size => {
                case Array(elements) if size == elements.length && elements.forall(e => e == Basic.Unit) => Basic.Unit
            })
        ).name("array.new", Types.ParameterizedRelation(Types.Int, Types.Unit, ArrayType)),
        self_inverse(
            int_parameter(index => {
                case Value.Pair(array: Array, write_value) =>
                    val read_value = array.elements(index)
                    val new_array = Array(array.elements.updated(index, write_value))
                    
                    Value.Pair(
                        new_array, read_value
                    )
            })
        ).name("array.readwrite", Types.ParameterizedRelation(Types.Int, Types.Pair(ArrayType, new Types.MetaVariable), Types.Pair(ArrayType, new Types.MetaVariable))),
    )
}
