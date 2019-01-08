package ltbs.uniform.common.web

import play.twirl.api.Html

import cats.implicits._
import ltbs.uniform.datapipeline._

trait Dom
trait Response

trait SimpleInteractionForm[IN,T,A,OUT] {
  def render(key: String, existing: Option[Encoded], data: IN, value: T,errors: ErrorTree): OUT
  def render(key: String, existing: Option[Encoded], data: IN, value: T): OUT =
    render(key, existing, data, value, Tree.empty)
  def receiveInput(data: IN): Encoded
  def decodeInput(data: IN): Either[ErrorTree,A] = decode(receiveInput(data))
  def encode(in: A): Encoded
  def decode(out: Encoded): Either[ErrorTree,A]
  
  def transform[B](f: A => Either[ErrorTree,B])(g: B => A) = {
    val fa = this
    new SimpleInteractionForm[IN,T,B,OUT] {
      def decode(out: Encoded): Either[ErrorTree,B] = fa.decode(out).flatMap(f)
      def receiveInput(data: IN): Encoded = fa.receiveInput(data)
      def encode(in: B): Encoded = fa.encode(g(in))
      def render(key: String, existing: Option[Encoded], data: IN, value: T, errors: ErrorTree): OUT =
        fa.render(key,existing,data, value, errors)
    }
  }

  def validating(f: A => Either[ErrorTree,A]): SimpleInteractionForm[IN,T,A,OUT] =
    transform(f)(identity)
}

trait JsForm[T,A] extends SimpleInteractionForm[Dom,T,A,Html] {
  override def render(key: String, existing: Option[Encoded], data: Dom, value: T, errors: ErrorTree): Html
  override def receiveInput(request: Dom): Encoded
  override def encode(in: A): Encoded
  override def decode(out: Encoded): Either[ErrorTree,A]
}


