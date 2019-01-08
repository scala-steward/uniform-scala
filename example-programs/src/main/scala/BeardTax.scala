package ltbs.uniform.sampleprograms

import cats.implicits._
import org.atnos.eff._
import ltbs.uniform._
import enumeratum._

object BeardTax { 

  case class MemberOfPublic(
    forename: String,
    surname: String,
    age: java.time.LocalDate
  )

  type BeardLength = (Int, Int)

  sealed trait BeardStyle extends EnumEntry
  object BeardStyle extends Enum[BeardStyle] {
    val values = findValues
    case object Goatee           extends BeardStyle
    case object Horseshoe        extends BeardStyle
    case object Gunslinger       extends BeardStyle
    case object MuttonChops      extends BeardStyle
    case object SoulPatch        extends BeardStyle
    case object LaughingCavalier extends BeardStyle
    case object LukaszStyle      extends BeardStyle                
  }

  type TestProgramStack = Fx1[
    // UniformInteraction[?,Unit,Option[MemberOfPublic]],    
    // UniformAsk[?,BeardStyle],
    UniformAsk[?,Int]
  ]

  def costOfBeard(length: BeardLength): Int =
    length._1 + (length._2 - length._1) / 2

  def program[R
      // : _uniform[Option[MemberOfPublic],?]
      // : _uniform[BeardStyle,?]
      : _uniform[Int,?]
  ]: Eff[R, Int] =
    for {
      // memberOfPublic <- interact[R, Unit, Option[MemberOfPublic]]("is-public", ())
      // beardStyle     <- ask[R, BeardStyle]("beard-style")            
      beardLength    <- interact[R, Unit, Int](
        "beard-length-mm",
        Unit).map(_._2)
    } yield costOfBeard((beardLength,0))


}
