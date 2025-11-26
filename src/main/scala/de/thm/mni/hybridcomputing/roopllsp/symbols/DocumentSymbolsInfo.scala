package de.thm.mni.hybridcomputing.roopllsp.symbols

class DocumentSymbolsInfo (val classes : Map[String, List[String]] = Map[String, List[String]]()) {
  override def toString: String = classes.toString()
}
