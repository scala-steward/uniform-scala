package ltbs.uniform

import org.scalatest._
import cats.implicits._
import cats.Monoid

class ValidationRuleSpec extends FlatSpec with Matchers {

  "ValidationRules" should "always return true if empty" in {
    List.empty[List[ValidationRule[Int]]].
      combinedValidation.
      errorsFor(12) should be (Nil)

    List(List.empty[ValidationRule[Int]]).
       combinedValidation.
       errorsFor(12) should be (Nil)
  }
}
