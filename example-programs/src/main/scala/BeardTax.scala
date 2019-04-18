package ltbs.uniform.sampleprograms

import cats.implicits._
import org.atnos.eff._
import ltbs.uniform._
import enumeratum._
import java.time.{LocalDate => Date}

object BeardTax {

  type BeardLength = (Int, Int)

  case class MemberOfPublic(
    forename: String,
    surname: String,
    age: Date
  )

  sealed trait BeardStyle extends EnumEntry
  object BeardStyle extends Enum[BeardStyle] {
    val values = findValues
    case object Goatee           extends BeardStyle
    case object Horseshoe        extends BeardStyle
    case object Gunslinger       extends BeardStyle
    case object MuttonChops      extends BeardStyle
    case object SoulPatch        extends BeardStyle
    case object LaughingCavalier extends BeardStyle
  }

  def costOfBeard(beardStyle: BeardStyle, length: BeardLength): Int =
    beardStyle match {
      case BeardStyle.SoulPatch => length._2 / 10
      case _                    => length._1 + (length._2 - length._1) / 2
    }

  type TestProgramStack = Fx3[
    UniformAsk[Option[MemberOfPublic], ?],
    UniformAsk[BeardStyle, ?],
    UniformAsk[BeardLength, ?]
  ]

  def program[R
      : _uniformCore
      : _uniformAsk[Option[MemberOfPublic],?]
      : _uniformAsk[BeardStyle,?]
      : _uniformAsk[BeardLength,?]
  ]: Eff[R, Int] =
    for {
      memberOfPublic <- ask[Option[MemberOfPublic]]("is-public")
        .validating( "born-in-future", { case Some(MemberOfPublic(_,_,bornOn)) if bornOn.isBefore(Date.now) => false; case _ => true} )
        .validating( "cant-be-called-fred", { case Some(MemberOfPublic("fred",_,_)) => false; case _ => true })
        .validating( "cant-be-called-smith", {case Some(MemberOfPublic(_,"smith",_)) => false; case _ => true} )
      beardStyle     <- ask[BeardStyle]("beard-style")
        .withCustomContentAndArgs(
          "beard-style.heading" -> {memberOfPublic match {
            case Some(MemberOfPublic(forename,_,_)) =>
              ("beard-style.heading.menacing", List(forename))
            case None =>
              ("beard-style.heading.sycophantic",Nil)
          }} )
      beardLength    <- ask[BeardLength]("beard-length-mm")
        .validating( "lower-exceeds-higher", {case (l,h) => h >= l})
        .emptyUnlessPred(memberOfPublic.isDefined)
    } yield costOfBeard(beardStyle, beardLength)

}
