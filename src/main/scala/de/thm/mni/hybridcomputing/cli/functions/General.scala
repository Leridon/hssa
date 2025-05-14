package de.thm.mni.hybridcomputing.cli.functions

import de.thm.mni.hybridcomputing.cli.CliChain.Value
import de.thm.mni.hybridcomputing.cli.{CliChain, Evaluation}
import de.thm.mni.hybridcomputing.cli.Evaluation.Arguments
import de.thm.mni.hybridcomputing.hssa.Formatting
import java.nio.file.Path

object General {
    
    def all: Seq[Evaluation.Function] = Seq(
        Load,
        Tap,
        Foreach,
        Dump,
        Drop,
    )
    
    import Evaluation.Function
    
    object Load extends Function("load") {
        override def instantiate(args: Arguments): CliChain.Function = {
            val path = args.expectPositionedString()
            
            val p = Path.of(path)
            
            _ => CliChain.Value.File.fromPath(p)
        }
    }
    
    object Tap extends Function("tap") {
        override def instantiate(args: Arguments): CliChain.Function = {
            import CliChain.Function.*
            val f = args.expectPositionedChain().withImplicitDump
            
            input => {
                f(input)
                
                input
            }
        }
    }
    
    object Foreach extends Function("foreach") {
        override def instantiate(args: Arguments): CliChain.Function = {
            import CliChain.Function.*
            val f = args.expectPositionedChain().withImplicitDump
            
            {
                case CliChain.Value.Sequence(seq) =>
                    seq.foreach(f)
                    
                    CliChain.Value.Unit
            }
        }
    }
    
    object Dump extends Function("dump") {
        override def instantiate(args: Arguments): CliChain.Function = this.apply
        def apply(input: CliChain.Value): CliChain.Value = {
            
            input match {
                case Value.Sequence(seq) => seq.foreach(this.apply)
                case Value.File(path, name, Some(in_memory_content)) =>
                    if (name.isDefined) println(s"File '${name.get}':")
                    
                    println(in_memory_content)
                case Value.HSSA(program) =>
                    this.apply(Value.File.fromContent(Formatting.format(program)))
                case in => println(in)
            }
            
            CliChain.Value.Unit
        }
    }
    
    object Drop extends Function("drop") {
        override def instantiate(args: Arguments): CliChain.Function = _ => Value.Unit
    }
    
    
}
