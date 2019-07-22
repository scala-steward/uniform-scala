package ltbs.uniform
package examples

import cats.Monad
import cats.implicits._
import scala.language.higherKinds
import java.time.{LocalDate => Date}

case class Familiar(
  name: String,
  age: Int
)

sealed trait WitchType
final case object OldHag extends WitchType
final case object Warlock extends WitchType
final case class NonHuman(description: String) extends WitchType

case class Witch (
  name: String,
  typeOfWitch: WitchType,
  pactWithDevilMadeOn: Option[Date],
  familiars: List[Familiar]
)

package object witchtax {

  type TellTypes = NilTypes
  type AskTypes = String :: WitchType :: Option[Date] :: List[Familiar] :: NilTypes

  def witchJourney[F[_]: Monad](
    interpreter: Language[F, TellTypes, AskTypes]
  ): F[Witch] = {
    import interpreter._
    (
      ask[String]("name"),
      ask[WitchType]("typeOfWitch"),
      ask[Option[Date]]("pactWithDevilMadeOn"),
      ask[List[Familiar]]("familiars")
    ).mapN(Witch.apply)
  }

}
