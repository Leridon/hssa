package de.thm.mni.hybridcomputing.cli.functions

import de.thm.mni.hybridcomputing.cli.CliChain
import de.thm.mni.hybridcomputing.cli.Evaluation
import de.thm.mni.hybridcomputing.cli.Evaluation.Arguments
import org.apache.commons.io.input.BufferedFileChannelInputStream
import java.nio.file.{Path, Paths}

object General {
    object Load extends Evaluation.Function("load") {
        override def instantiate(args: Arguments): CliChain.Function = {
            val path = args.expectPositionedString()
            
            val p = Path.of(path)
            
            _ => CliChain.Value.File.fromPath(p)
        }
    }
}
