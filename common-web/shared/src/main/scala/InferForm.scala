package ltbs.uniform.web

import play.twirl.api.Html
import shapeless._
import shapeless.labelled._
import cats.implicits._
import cats.Monoid
import ltbs.uniform.ErrorTree

trait HtmlField[A]{
  def render(key: String, values: Input, errors: ErrorTree, messages: Messages): Html
}

trait HtmlForm[A]{
  def render(key: String, values: Input, errors: ErrorTree, messages: Messages): Html
}

trait InferForm {

  def errorSummary(key: String, values: Input, errors: ErrorTree, messages: Messages): Html  
  def soloField(key: String, values: Input, errors: ErrorTree, messages: Messages)(inner: Html): Html
  def compoundField(key: String, values: Input, errors: ErrorTree, messages: Messages)(inner: Html): Html

  implicit val hnilField = new HtmlField[HNil] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages): Html =
      Monoid[Html].empty
  }

  implicit def hConsField[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hField: Lazy[HtmlField[H]],
    tField: HtmlField[T]
  ): HtmlField[FieldType[K,H] :: T] = new HtmlField[FieldType[K,H] :: T] {
    val fieldName: String = witness.value.name

    def render(key: String, values: Input, errors: ErrorTree, messages: Messages): Html =
      compoundField(s"${key}.${fieldName}", values, errors, messages)(
      hField.value.render(s"${key}.${fieldName}", values, errors, messages)) |+|
        tField.render(key, values, errors, messages)
  }

  implicit def genericField[A, H, T]
    (implicit
       generic: LabelledGeneric.Aux[A,T],
     hGenParser: Lazy[HtmlField[T]]
    ): HtmlField[A] = new HtmlField[A]
  {

    def render(key: String, values: Input, errors: ErrorTree, messages: Messages): Html =
      hGenParser.value.render(key, values, errors, messages)
  }

  implicit def simpleForm[A](implicit field: HtmlField[A]): HtmlForm[A] = new HtmlForm[A] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages): Html =
      errorSummary(key, values, errors, messages) |+|
      soloField(key, values, errors, messages)(field.render(key, values, errors, messages))
  }

  // implicit def compoundForm[A, H, T]
  //   (implicit
  //      generic: LabelledGeneric.Aux[A,T],
  //    hGenParser: Lazy[HtmlField[T]]
  //   ): HtmlForm[A] =
  //   new HtmlForm[A] {
  //     def render(key: String, values: Input, errors: Error, messages: Messages): Html =
  //     errorSummary(key, values, errors, messages) |+|
  //       soloField(key, values, Tree(errors.value), messages)(
  //         genericField(generic, hGenParser).render(key, values, errors, messages)
  //       )
  //   }

}