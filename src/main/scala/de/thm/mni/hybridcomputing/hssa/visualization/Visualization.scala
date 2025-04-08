package de.thm.mni.hybridcomputing.hssa.visualization

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.BindingTree.Block.VariableRole
import de.thm.mni.hybridcomputing.hssa.{BindingTree, CallGraph, Formatting, Syntax}

object Visualization {
    object ControlFlowGraphVisualization {
        def apply(t: hssa.BindingTree.Relation): String = {
            
            def node_name(block: BindingTree.Block): String = s"block${t.blocks.indexOf(block)}"
            
            val nodes = t.blocks.map(b => s"${node_name(b)} [shape=\"box\",label=\"${Formatting.format(b.syntax).replace("\n", "\\n")}\"];")
            
            val edges = t.labels.flatMap(l => {
                for (from <- t.getAllExits(l.name);
                     to <- t.getAllEntries(l.name)
                     ) yield s"${node_name(from.block)} -> ${node_name(to.block)} [label=\"${l.name}\",taillabel=${from.index},headlabel=${to.index}]"
            })
            
            s"""digraph ${t.syntax.name} {
               |graph [fontname = "Courier New"];
               |node [fontname = "Courier New"];
               |edge [fontname = "Courier New"];
               |${nodes.mkString("\n")}
               |${edges.mkString("\n")}
               |}
               |""".stripMargin
        }
    }
    
    object CallGraphVisualization {
        def apply(t: hssa.BindingTree.Program): String = {
            val call_graph = CallGraph.get(t)
            
            val nodes = t.relations.map(b => s"\"${b.name}\" [shape=\"box\"];")
            
            val edges = call_graph.unique_calls.map(call => {
                s"\"${call.caller.syntax.name}\" -> \"${call.callee.syntax.name}\""
            })
            
            s"""digraph {
               |graph [fontname = "Courier New"];
               |node [fontname = "Courier New"];
               |edge [fontname = "Courier New"];
               |${nodes.mkString("\n")}
               |${edges.mkString("\n")}
               |}
               |""".stripMargin
        }
    }
    
    object BlockCircuitVisualization {
        def apply(t: hssa.BindingTree.Block): String = {
            val variable_nodes = t.block_local_variables.map(s => {
                s"\"$s\" [shape=plaintext]"
            })
            
            val assignment_nodes = t.syntax.sequence.zipWithIndex.map({ case (s, i) =>
                s"\"__stm$i\" [shape=box,label=\"${Formatting.format(s)}\"]"
            })
            
            val edges = t.all_variable_usages.values.flatten.filter(v => v.ref.exists(_.isInstanceOf[BindingTree.BlockVariable])).map(usage => {
                usage.role match
                    case VariableRole.Init => s"__stm${usage.statement_index} -> \"${usage.variable.name}\""
                    case VariableRole.Final => s"\"${usage.variable.name}\" -> __stm${usage.statement_index}"
                    case VariableRole.Use => s"\"${usage.variable.name}\" -> __stm${usage.statement_index} [style=dashed]"
            })
            
            s"""digraph {
               |graph [fontname = "Courier New"];
               |node [fontname = "Courier New"];
               |edge [fontname = "Courier New"];
               |${variable_nodes.mkString("\n")}
               |${assignment_nodes.mkString("\n")}
               |${edges.mkString("\n")}
               |}
               |""".stripMargin
        }
    }
}