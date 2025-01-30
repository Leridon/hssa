package de.thm.mni.hybridcomputing.util.parsing

import scala.compiletime.uninitialized

trait Positioned {
    private var _position: SourcePosition = uninitialized
    
    def setPosition(pos: SourcePosition): this.type = {
        this._position = pos
        this
    }
    
    def position: SourcePosition = this._position
}