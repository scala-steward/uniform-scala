package ltbs.uniform

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers
import cats.implicits._
import cats.Monad
import scala.language.higherKinds
import validation.Rule

class RewriterSpec extends AnyFlatSpec with Matchers with OptTCOps {

  "A rewriter" should "change a journey" in {

    type TellTypes = NilTypes
    type AskTypes = String :: Int :: NilTypes

    def program[F[_]: Monad](
      interpreter: Language[F, TellTypes, AskTypes]
    ): F[(String, Int)] = {
      import interpreter._
      for {
        s <- ask[String]("s")
        i <- ask[Int]("i")
      } yield (s,i)
    }

    object MyRewriter extends Rewriter (
      new MonoidInterpreter[TellTypes, AskTypes]
    ) {
      trait One[A] extends RW[A]

      implicit val oneString: One[String] = new One[String] {
        def interact[Tell](
          id: String,
          tell: Tell,
          default: Option[String] = None,
          validation: List[Rule[String]] = Nil,
          customContent: Map[String,(String,List[Any])] = Map.empty
        ): String = naive.ask[Int]("test").map{_.toString}
      }
    }

    program(MyRewriter.rewrite[MyRewriter.One]) should be (("0",0))
  }
}
