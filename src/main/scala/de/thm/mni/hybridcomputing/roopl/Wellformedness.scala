package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.roopl.Syntax.ClassIdentifier
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}

// Step 2 of semantic analysis
object Wellformedness {

    private object Internal {
        def check(program: BindingTree.Program, errors: LanguageError.Collector): Unit = {
            // No duplicate classNames
            program.classes.foreach((k,v) => v.tail.foreach(c => errors.add(DuplicateClassName(k, c.syntax))))
        }

        def check(classDefinition: Syntax.ClassDefinition, collector: LanguageError.Collector): Unit = {
            
        }
    }
    
    def check(program: Syntax.Program): Unit = {
        val context = BindingTree.Program(program)

        val collector = LanguageError.Collector()
        
        Internal.check(context, collector)
        
        collector.raiseIfNonEmpty()
    }

    case class DuplicateClassName(name: ClassIdentifier, definition: Syntax.ClassDefinition) extends RooplError(Error, s"class $name is already defined.", definition.position)
}