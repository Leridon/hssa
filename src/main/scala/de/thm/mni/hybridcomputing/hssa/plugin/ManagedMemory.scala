package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.Language.Plugin.Builtin
import de.thm.mni.hybridcomputing.hssa.interpretation.Value.BuiltinRelation
import de.thm.mni.hybridcomputing.hssa.plugin.BuiltinCreationHelpers.unit_input
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.Types
import de.thm.mni.hybridcomputing.hssa.plugin.BuiltinCreationHelpers.int_parameter
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation
import de.thm.mni.hybridcomputing.hssa.plugin.BuiltinCreationHelpers.self_inverse
import de.thm.mni.hybridcomputing.hssa.plugin.BuiltinCreationHelpers.FunctionPair
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.Errors.RuntimeError

object ManagedMemory extends Plugin {

    case class MMem(content: Array[Value]) extends Value {
        override def toString(): String = {
            val allocatedMemory = content.slice(0, content.lastIndexWhere(_ != null) + 1)
            val indexSize = allocatedMemory.size.toString().length()
            allocatedMemory.zipWithIndex.map((value, index) =>
                s"${String.format(s"%${indexSize}s", index)} => $value"
            ).mkString("\n")
        }
    }

    object MMemType extends Types.Type

    // Limit the number of objects in managed memory to 1024
    val memorySize: Int = 1024

    override def builtins: Seq[Builtin] = Seq(
        FunctionPair(
            { case Basic.Unit => unit_input({MMem(new Array[Value](memorySize))})},
            { case Basic.Unit => {
                case MMem(content) if content.forall(_ == null) => Basic.Unit
                case _ => Errors.UnallocationError().raise()
            }}
        ).name("mmem.new", Types.ParameterizedRelation(Types.Unit, Types.Unit, MMemType)),
        FunctionPair(
            int_parameter(size => {
                case memory: MMem => {
                    if size == 0 then Errors.ZeroAllocationError().raise()
                    // tail to protect null pointer
                    val pointer = memory.content.tail.sliding(size).zipWithIndex.find {
                        case (w, _) if w.length == size => w.forall(_ == null)
                    }.map(_._2).getOrElse(Errors.AllocationError().raise()) + 1
                    
                    for (i <- Range(pointer, pointer + size)) {
                        memory.content.update(i, Basic.Int(0))
                    }

                    Value.Pair(
                        memory, Basic.Int(pointer)
                    )
                }
            }),
            int_parameter(size => {
                case Value.Pair(memory: MMem, pointer: Basic.Int)
                    if memory.content.slice(pointer.value, pointer.value + size).forall(_ == Basic.Int(0)) => {
                        if pointer.value == 0 then Errors.NullPointerError("deallocate").raise()
                        if size == 0 then Errors.ZeroAllocationError().raise()
                        for (i <- Range(pointer.value, pointer.value + size)) {
                            memory.content.update(i, null)
                        }
                        memory
                    }
            })
        ).name("mmem.allocate", Types.ParameterizedRelation(Types.Int, MMemType, Types.Pair(MMemType, Types.Int))),
        self_inverse(
            int_parameter(pointer => {
                case Value.Pair(memory: MMem, in: Value) => {
                    if pointer == 0 then Errors.NullPointerError("read/write").raise()
                    val read_value = memory.content(pointer)
                    if (read_value == null) {
                        Errors.AccessViolation().raise()
                    }
                    memory.content.update(pointer, in)

                    Value.Pair(
                        memory, read_value
                    )
                }
            })
        ).name("mmem.readwrite", Types.ParameterizedRelation(Types.Int, Types.Pair(MMemType, new Types.MetaVariable), Types.Pair(MMemType, new Types.MetaVariable)))
    )
    
    object Errors {
        case class AllocationError() extends RuntimeError("Unable to allocate enough space in managed memory!")
        case class ZeroAllocationError() extends RuntimeError("Must allocate more than 0 slots!")
        case class AccessViolation() extends RuntimeError("Trying to access unallocated memory!")
        case class UnallocationError() extends RuntimeError("Trying to unallocate non zero-cleared memory!")
        case class NullPointerError(op: String) extends RuntimeError(s"Trying to $op nullpointer!")
    }
}