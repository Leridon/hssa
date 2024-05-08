package de.thm.mni.hssa

import de.thm.mni.hssa.Syntax.Program

object Inversion {
    case class InversionContext(
                                 inverted_relations: List[String],
                               ) {
        def inverts(relation: String): Boolean = inverted_relations.contains(relation)
    }
    
    object InversionContext {
        def all(program: Program): InversionContext = InversionContext(program.definitions.map(_.name))
        def only(relations: List[String]): InversionContext = InversionContext(relations)
    }
    
    def invert(statement: Syntax.Statement, ctx: InversionContext): Syntax.Statement = statement match {
        case Syntax.Assignment(target, inverted, relation_name, instance_argument, source) =>
            // TODO: When relations are first-class types, inversion may have no solution!
            
            if (ctx.inverts(relation_name)) Syntax.Assignment(source, inverted, relation_name, instance_argument, target)
            else Syntax.Assignment(source, !inverted, relation_name, instance_argument, target)
        case Syntax.UnconditionalExit(target, argument) =>
            Syntax.UnconditionalEntry(argument, target)
        case Syntax.ConditionalExit(target1, target2, argument) =>
            Syntax.ConditionalEntry(argument, target1, target2)
        case Syntax.UnconditionalEntry(initialized, target) =>
            Syntax.UnconditionalExit(target, initialized)
        case Syntax.ConditionalEntry(initialized, target1, target2) =>
            Syntax.ConditionalExit(target1, target2, initialized)
    }
    
    def invert(relation: Syntax.Relation, ctx: InversionContext): Syntax.Relation =
        Syntax.Relation(relation.name, relation.parameter, relation.body.map(stm => invert(stm, ctx)).reverse)
    
    def invert(program: Program, ctx: InversionContext = null): Syntax.Program = {
        val context = if (ctx == null) InversionContext.all(program) else ctx
        
        Syntax.Program(program.definitions.map(invert(_, context)))
    }
}
