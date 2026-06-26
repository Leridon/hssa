package de.thm.mni.hybridcomputing.hssa.util

import de.thm.mni.hybridcomputing.hssa.{Language, Syntax}
import de.thm.mni.hybridcomputing.util.UniqueNameGenerator

import scala.collection.mutable.ListBuffer

class ProgramBuilder(val language: Language) {
    val relation_name_generator: UniqueNameGenerator = new UniqueNameGenerator()
      .withExternalReservation(n => language.builtins.exists(b => b.value.name == n))

    private val relations = new ListBuffer[RelationBuilder]

    def newRelation(name: String, parameter: Syntax.Expression): RelationBuilder = {
        val rel = new RelationBuilder(name, parameter)
        this.relations.addOne(rel)
        rel
    }

    def add(rel: Syntax.Relation): RelationBuilder = {
        val r = new RelationBuilder(rel)
        this.relations.addOne(r)
        r
    }

    def compile(): Syntax.Program = Syntax.Program(relations.toSeq.map(_.compile()), language)
}
