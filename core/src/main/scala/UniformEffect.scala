package ltbs

import org.atnos.eff._
import cats.data.Validated
import org.atnos.eff.all.{none => _, _}
import cats.implicits._
import cats.Monoid

package object uniform {

  type _uniform[V,S] = UniformInteraction[?,Unit,V] |= S
  type _uniformSelect[V,R] = UniformSelect[V,?] |= R

  type UniformAsk[STACK, A] = UniformInteraction[STACK, Unit, A]
  type UniformTell[STACK, A] = UniformInteraction[STACK, A, Unit]
  type UniformEnd[STACK, A] = UniformInteraction[STACK, A, Nothing]    

  def interact[R, TELL, ASK](
    key: String,
    value: TELL,
    validation: ASK => Validated[String,ASK] = {v:ASK => v.valid}
  )(
    implicit member: UniformInteraction[?, TELL, ASK] |= R
  ): Eff[R, (TELL,ASK)] = send[UniformInteraction[?, TELL, ASK], R, (TELL,ASK)](
    UniformInteraction(key, value, validation)
  )

  def ask[R, T](
    key: String,
    validation: T => Validated[String,T] = {v:T => v.valid}
  )(
    implicit member: UniformInteraction[?, Unit, T] |= R
  ): Eff[R, (Unit,T)] = interact[R, Unit, T](key, (), validation)

  def tell[R, T](
    key: String,
    value: T
  )(
    implicit member: UniformInteraction[?, T, Unit] |= R
  ): Eff[R, (T,Unit)] = interact[R, T, Unit](key, value)

  def end[R, T](
    key: String,
    value: T
  )(
    implicit member: UniformInteraction[?, T, Nothing] |= R
  ): Eff[R, (T,Nothing)] = interact[R, T, Nothing](key, value)

  def askOneOf[R, T](
    key: String,
    options: Set[T],
    validation: T => Validated[String,T] = {v:T => v.valid}
  )(implicit member: UniformSelect[T, ?] |= R): Eff[R, T] =
    send[UniformSelect[T, ?], R, T](UniformSelectOne(key, options, validation))

  def askNOf[R, T](
    key: String,
    options: Set[T],
    min: Int = 0,
    max: Int = Int.MaxValue,
    validation: Set[T] => Validated[String,Set[T]] = {v:Set[T] => v.valid}
  )(implicit member: UniformSelect[T, ?] |= R): Eff[R, Set[T]] =
    send[UniformSelect[T, ?], R, Set[T]](
      UniformSelectMany(key, options, min, Math.min(max, options.size), validation)
    )

  implicit class RichMonoidOps[R, A](e: Eff[R, A])(implicit monoid: Monoid[A]) {
    
    def emptyUnless(b: => Boolean): Eff[R, A] =
      if(b) e else Eff.pure[R,A](monoid.empty)

    def emptyUnless(eb: Eff[R,Boolean]): Eff[R,A] = for {
      opt <- eb
      ret <- if (opt) e else Eff.pure[R,A](monoid.empty)
    } yield ret

  }

  implicit class RichOps[R, A](wm: Eff[R, A]) {
    def when(b: => Boolean): Eff[R,Option[A]] =
      if(b) wm.map{_.some} else Eff.pure[R,Option[A]](none[A])

    def when(wmb: Eff[R,Boolean]): Eff[R,Option[A]] = for {
      opt <- wmb
      ret <- if (opt) wm map {_.some} else Eff.pure[R,Option[A]](none[A])
    } yield ret
  }

  def when[R, A](b: => Boolean)(wm: Eff[R, A]): Eff[R,Option[A]] =
    if(b) wm.map{_.some} else Eff.pure[R,Option[A]](none[A])

}
