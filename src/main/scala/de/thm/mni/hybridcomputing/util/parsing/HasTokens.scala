package de.thm.mni.hybridcomputing.util.parsing

import scala.compiletime.uninitialized

trait HasTokens[T] {
    private var _tokens: Seq[Token[T]] = uninitialized
    
    def tokens: Seq[Token[T]] = this._tokens
    
    def hasTokens: Boolean = this._tokens != null
    
    def setTokens(tokens: Seq[Token[T]]): this.type = {
        this._tokens = tokens
        this
    }
}
