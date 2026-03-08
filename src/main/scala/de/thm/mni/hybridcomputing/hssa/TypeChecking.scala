package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity

import scala.collection.mutable

class TypeChecking(language: Language) {
    class TypeError(expected: Types.Type, actual: Types.Type) extends HSSAError(Severity.Error, s"Type Error: Expected $expected, but got $actual")
    
    class Environment(prog: BindingTree.Program) {
        private val map = mutable.Map[Environment.Key, Types.Type]()
        
        def set(key: Environment.Key, t: Types.Type): Unit = map.update(key, t)
        def get(key: Environment.Key, fallback: () => Types.Type = () => new Types.MetaVariable): Types.Type = map.getOrElseUpdate(key, fallback())
        
        def print(detail: Int = 3): Unit = {
            
            prog.builtins.foreach(builtin => {
                println(s"${builtin.name}: ${this.get(Environment.VariableKey(builtin))}")
            })
            
            prog.relations.foreach(rel => {
                println(s"${rel.relation.syntax.name}: ${this.get(Environment.VariableKey(rel))}")
                
                if (detail >= 1) {
                    rel.relation.labels.foreach(label => {
                        println(s"  ${label.name}: ${this.get(Environment.LabelKey(label))}")
                    })
                }
            })
            
            
        }
    }
    
    object Environment {
        sealed trait Key {
            override def toString: String = this match
                case ExpressionKey(expression) => expression.toString
                case VariableKey(variable) => variable.toString
                case LabelKey(label) => label.toString
        }
        case class ExpressionKey(expression: Syntax.Expression) extends Key {
            override def equals(other: Any): Boolean = {
                other match
                    case ExpressionKey(o) => o eq expression
                    case _ => false
            }
        }
        case class VariableKey(variable: BindingTree.Variable) extends Key {
        
        }
        case class LabelKey(label: BindingTree.Label) extends Key {
        
        }
    }
    
    def check(program: hssa.BindingTree.Program): LanguageError.Collector = {
        val collector = LanguageError.Collector()
        
        val env = new Environment(program)
        
        program.builtins.foreach(builtin => env.set(Environment.VariableKey(builtin), builtin.builtin.`type`))
        
        def botup(environment: BindingTree, exp: Expression): Types.Type =
            def helper(exp: Expression): Types.Type = {
                exp match
                    case Expression.Literal(value) => Types.Int
                    case Expression.Variable(name) =>
                        val variable = environment.lookup(name.name).get
                        
                        variable match
                            case BindingTree.GlobalBuiltinVariable(name, program, builtin) =>
                                env.get(Environment.VariableKey(variable), () => builtin.`type`)
                            case _ =>
                                env.get(Environment.VariableKey(variable))
                    case Expression.Pair(a, b) => Types.Pair(helper(a), helper(b))
                    case Expression.Invert(a) =>
                        val t = helper(a)
                        val rel = Types.ParameterizedRelation(new Types.MetaVariable(), new Types.MetaVariable(), new Types.MetaVariable())
                        
                        if (Types.unify(t, rel).isEmpty) {
                            collector.add(TypeError(rel, t)
                              .setPosition(a.position)
                            )
                        }
                        
                        Types.ParameterizedRelation(rel.parameter, rel.out, rel.in)
                    case Expression.Unit() => Types.Unit
            }
            
            env.get(Environment.ExpressionKey(exp), () => helper(exp))
        
        program.relations.foreach(rel => {
            // Type the parameter
            val par_type = botup(
                rel.relation,
                rel.relation.syntax.parameter
            )
            
            rel.relation.labels.foreach(label => {
                env.set(Environment.LabelKey(label),
                    new Types.MetaVariable
                )
            })
            
            val in_type = env.get(Environment.LabelKey(rel.relation.labels.find(l => l.name == Language.BeginLabel).get))
            val out_type = env.get(Environment.LabelKey(rel.relation.labels.find(l => l.name == Language.EndLabel).get))
            
            env.set(Environment.VariableKey(rel),
                Types.ParameterizedRelation(par_type, in_type, out_type)
            )
        })
        
        val call_graph = CallGraph.get(program)
        
        call_graph.topologically_sorted.foreach(rel => {
            rel.blocks.foreach(block => {
                {
                    val t = botup(block, block.syntax.entry.initialized)
                    
                    val t2 = Types.Pair(block.entry_labels.map(l => {
                        env.get(Environment.LabelKey(l))
                    }).reduce(Types.UnionType.apply), Types.Int)
                    
                    if (Types.unify(t, t2).isEmpty) {
                        collector.add(
                            TypeError(t, t2)
                              .setPosition(block.syntax.entry.initialized.position)
                        )
                    }
                }
                
                block.syntax.assignments.foreach(assignment => {
                    val t1 = botup(block, assignment.target)
                    val t2 = botup(block, assignment.relation)
                    val t3 = botup(block, assignment.instance_argument)
                    val t4 = botup(block, assignment.source)
                    
                    val rel_t = Types.ParameterizedRelation(new Types.MetaVariable, new Types.MetaVariable, new Types.MetaVariable)
                    
                    if (Types.unify(Types.clone(t2), rel_t).isEmpty) {
                        collector.add(
                            TypeError(t2, rel_t)
                              .setPosition(assignment.relation.position)
                        )
                    }
                    
                    if (Types.unify(t3, rel_t.parameter).isEmpty) {
                        collector.add(
                            TypeError(rel_t.parameter, t3)
                              .setPosition(assignment.instance_argument.position)
                        )
                    }
                    
                    if(Types.unify(t1, rel_t.out).isEmpty) {
                        collector.add(
                            TypeError(rel_t.out, t1)
                              .setPosition(assignment.target.position)
                        )
                    }
                    
                    if(Types.unify(t4, rel_t.in).isEmpty) {
                        collector.add(
                            TypeError(t4, rel_t.in)
                              .setPosition(assignment.source.position)
                        )
                    }
                })
                
                {
                    val t = botup(block, block.syntax.exit.argument)
                    
                    val t2 = Types.Pair(block.exit_labels.map(l => {
                        env.get(Environment.LabelKey(l))
                    }).reduce(Types.UnionType.apply), Types.Int)
                    
                    if (Types.unify(t, t2).isEmpty) {
                        collector.add(
                            TypeError(t, t2)
                              .setPosition(block.syntax.exit.argument.position)
                        )
                    }
                }
            })
        })
        
        collector
    }
}
