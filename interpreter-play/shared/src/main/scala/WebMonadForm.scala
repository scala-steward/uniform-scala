package ltbs.uniform.interpreters.playframework

import cats.data.Validated
import play.twirl.api.Html
import play.api.data.Form
import play.api.mvc.{ Request, AnyContent }
import ltbs.uniform.datapipeline._
import ltbs.uniform.common.web._

trait WebMonadForm[T,A] {
  def render(key: String, existing: Input, tell: T, errors: ErrorTree, breadcrumbs: List[String]): Html
  def fromRequest(key: String, request: Request[AnyContent]): Either[ErrorTree, A]
  def encode(in: A): Encoded
  def decode(out: Encoded): Either[ErrorTree,A]
  def toTree(in: A): Input

  def bind(in: Input): Either[Error,A]
  def unbind(a:A): Input
}

trait WebMonadSelectPage[A] {
  def toHtml(in: A): Html
  def renderOne(key: String, options: Set[A], existing: ValidatedData[A], request: Request[AnyContent]): Html
  def renderMany(key: String, options: Set[A], existing: ValidatedData[Set[A]], request: Request[AnyContent]): Html
  def encode(in: A): Encoded
  def decode(out: Encoded): A
  def playFormOne(key: String, validation: A => Validated[ValidationError, A]): Form[A]
  def playFormMany(key: String, validation: Set[A] => Validated[ValidationError, Set[A]]): Form[Set[A]]
}
