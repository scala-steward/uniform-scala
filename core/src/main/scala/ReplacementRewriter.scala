package ltbs.uniform

import language.higherKinds
import shapeless._

abstract class ReplacementRewriter[A, TC[_], SupportedTell <: HList, SupportedAsk <: HList](
  val naive: Language[TC, SupportedTell, SupportedAsk]
) {

}
