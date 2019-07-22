package ltbs.uniform
package interpreters.playframework

import cats.implicits._
import cats.Monoid
import play.api._,mvc._,http.Writeable
import concurrent.{Future,ExecutionContext}
import java.util.UUID
import cats.data.{EitherT, RWST}
import common.web._

sealed trait ListAction
object ListAction {
  final case object Continue extends ListAction
  final case object Add extends ListAction  
  final case class Edit(ordinal: Int) extends ListAction
  final case class Delete(ordinal: Int) extends ListAction
}

abstract class ListingPages[F[_], A, Html: Writeable: Monoid](
  addJourney: WebMonad[A],
  editJourney: (A, Int) => WebMonad[A],
  deleteJourney: (A, Int) => WebMonad[Boolean],
  lowlevel: LowLevelDbAccess
)(
  implicit ec: ExecutionContext,
  listingJourneyAsk: GenericWebAsk[ListAction, Html],
  listingJourneyTell: GenericWebTell[List[A], Html]
) extends GenericWebAsk[List[A], Html] {

  def combinedJourney(default: List[A] = Nil): WebMonad[List[A]] = {
    import ListAction._
    // listingJourney(default) flatMap {
    //   case Continue => default.pure[WebMonad]
    //   case Add => addJourney.flatMap{n => combinedJourney(default :+ n)}
    //   case Edit(ordinal: Int) => ???
    //   case Delete(ordinal: Int) => ???
    // }
    ???
  }

}

object AutoListing {


}
