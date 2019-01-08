package ltbs.uniform.common

import cats.implicits._
import cats.arrow.Profunctor
import cats.Invariant
import ltbs.uniform.datapipeline._
import play.twirl.api.Html

package object web {
  type Encoded = String

  implicit def sifProfunctor[T,AS] = new Profunctor[SimpleInteractionForm[?,T,AS,?]] {
    def dimap[A, B, C, D](fab: SimpleInteractionForm[A,T,AS,B])(f: C => A)(g: B => D) =
      new SimpleInteractionForm[C,T,AS,D] {
        def decode(out: Encoded): Either[ErrorTree,AS] = fab.decode(out)
        def receiveInput(data: C): Encoded = fab.receiveInput(f(data))
        def encode(in: AS): Encoded = fab.encode(in)
        def render(key: String,existing: Option[Encoded], data: C, tell: T, errors: ErrorTree): D =
          g(fab.render(key,existing,f(data), tell, errors))
      }
  }

  implicit def sifAskInvariant[IN,OUT,TELL] = new Invariant[SimpleInteractionForm[IN,TELL,?,OUT]] {
    def imap[A, B](fa: SimpleInteractionForm[IN,TELL,A,OUT])(f: A => B)(g: B => A) =
      fa.transform(f map (_.asRight))(g)
  }

  def UrlEncodedHtmlForm[T,A](
    parser: DataParser[A],
    html: HtmlForm[T,A],
    messages: Messages
  ): SimpleInteractionForm[FormUrlEncoded,T,A,Html] = { 
    val underlying = new InputHtmlForm(parser, html, messages)
    sifProfunctor[T,A].lmap(underlying)(underlying.formToInput)
  }

}
