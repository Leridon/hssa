package de.thm.mni.hybridcomputing.util.parsing

import scala.compiletime.uninitialized

trait Positioned {
    var position: SourcePosition = uninitialized
}
