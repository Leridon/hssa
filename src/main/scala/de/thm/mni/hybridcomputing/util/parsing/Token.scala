package de.thm.mni.hybridcomputing.util.parsing

case class Token[T](typ: T, value: Option[Any] = None, lexeme: String) extends Positioned {
    override def toString: String = value match {
        case Some(value) => s"$typ($value)"
        case None => s"$typ"
    }
    
    def asStringWithPosition: String = {
        if(position == null) return this.toString

        value match {
            case Some(value) => s"$typ($value) at ${position.toString}"
            case None => s"$typ at ${position.toString}"
        }
    }
}
