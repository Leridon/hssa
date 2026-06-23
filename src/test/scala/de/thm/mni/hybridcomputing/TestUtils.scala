package de.thm.mni.hybridcomputing

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import org.scalactic.source
import org.scalatest.Tag
import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.wordspec.AnyWordSpec

object TestUtils {
    object OptionalTest extends Tag("OptionalTest")

    trait OptionalTests {
        this: AnyFlatSpec =>

        def optionally(test: => Unit): Unit = {
            try {
                test
            } catch {
                case e: TestFailedException =>
                    cancel(s"optional test failed: ${e.getMessage}")
            }
        }

        def optional(name: String, body: => Unit): Unit = {
            it should name in optionally(body)
        }
    }

    trait OptionalTestsAnyWord {
        this: org.scalatest.wordspec.AnyWordSpec =>

        def optionally(test: => Unit): Unit =
            try test
            catch {
                case e: Throwable =>
                    cancel(s"optional test failed: ${e.getMessage}")
            }

        def optional(name: String)(body: => Unit): Unit =
            name in optionally(body)
    }

    def wrapErrorPrint(f: => Unit): Unit = try {
        f
    } catch {
        case e: LanguageError.AbortDueToErrors =>
            e.errors.foreach(e => {
                println(e)
                println()
            })
            throw e
    }
}
