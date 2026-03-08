package de.thm.mni.hybridcomputing
import de.thm.mni.hybridcomputing.util.FormattingUtilities
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table

class CountParensTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
    
    private val cases = Table(
        ("input", "expected"),
        ("(a)", 1),
        ("(a, b)", 1),
        ("((a, b))", 2),
        ("((a), (b))", 1),
    )
    
    "count_parens" should "count parentheses correctly" in {
        forAll(cases) { (input, expected) =>
            FormattingUtilities.count_parens(input) shouldBe expected
        }
    }
}