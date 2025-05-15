package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.Language.Plugin.Builtin
import de.thm.mni.hybridcomputing.hssa.Types
import de.thm.mni.hybridcomputing.hssa.interpretation.Value

object BuiltinCreationHelpers {
    type InstantiatedFunction = Value => Value
    type Function = Value => InstantiatedFunction
    
    case class FunctionPair(
                             forward: Function,
                             backward: Function,
                           ) {
        
        def name(name: String, `type`: Types.ParameterizedRelation = Types.ParameterizedRelation(new Types.MetaVariable, new Types.MetaVariable, new Types.MetaVariable)): Builtin = Builtin(Value.BuiltinRelation(name, forward, backward), `type`)
    }
    
    def self_inverse(f: Function): FunctionPair = FunctionPair(f, f)
    
    def int_parameter(f: Int => Value => Value): Function = {
        case Basic.Int(i) => f(i)
    }
    
    def unit_input(f: => Value): InstantiatedFunction = {
        case Basic.Unit => f
    }
    
    def consume(expected_value: Value): InstantiatedFunction = {
        case v if v == expected_value => Basic.Unit
    }
    
    def produce(value: Value): InstantiatedFunction = {
        case Basic.Unit => value
    }
}
