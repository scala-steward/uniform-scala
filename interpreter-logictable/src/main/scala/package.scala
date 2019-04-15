package ltbs.uniform.interpreters

import scala.language.implicitConversions

import ltbs.uniform._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats.data.{Writer,State,NonEmptyList}
import cats.implicits._ // needed for scala 2.11 either flatMap

package object logictable {

  type LogicTableStack = Fx.fx4[
    State[UniformCore, ?],
    Either[NonEmptyList[ErrorMsg],?],
    Writer[String,?],
    List
  ]

  type Examples[T] = Function[String, List[T]]

  implicit def listToExamples[A](in: List[A]): Examples[A] = _ => in
  implicit def partialToExamples[A](in: PartialFunction[String,List[A]]): Examples[A] = x => in(x)

  implicit class UniformListEffectOps[R, A](e: Eff[R, A]) {
    type _either[Q] = Either[NonEmptyList[ErrorMsg],?] |= Q
    type _writer[Q] = Writer[String,?] |= Q

    def giveExamples[OUT, U : _either : _writer : _list](
      reader: Examples[OUT]
    )(
      implicit member: Member.Aux[Uniform[Unit,OUT,?], R, U]
    ): Eff[U, A] =
      e.translate(
        new Translate[Uniform[Unit,OUT,?], U] {
          def apply[X](ax: Uniform[Unit,OUT,X]): Eff[U, X] =
            ax match {
              case Uniform(key,_,_,v,_) =>
                val i: Eff[U,X] = for {
                  a <- ListEffect.values(reader(key.mkString("/")):_*)
                  _ <- WriterEffect.tell(s"${key.mkString(".")}:$a")
                  va <- send(v.combinedValidation(a).toEither.map{_.asInstanceOf[X]})
                } yield (va)
                i
            }
        })
  }
}
