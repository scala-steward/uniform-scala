package ltbs.uniform

import cats.implicits._
import cats.{Invariant,Monoid}
import scala.language.implicitConversions

package object web {

  type FormUrlEncoded = Map[String, Seq[String]]
  type Input = Tree[String, List[String]]

  implicit def richFormUrlEncoded(in: FormUrlEncoded): RichFormUrlEncoded =
    new RichFormUrlEncoded(in)

  implicit def sifInvariant[IN,OUT,TELL] = new Invariant[SimpleInteractionForm[IN,TELL,?,OUT]] {
    def imap[A, B](fa: SimpleInteractionForm[IN,TELL,A,OUT])(f: A => B)(g: B => A) =
      fa.transform(f map (_.asRight))(g)
  }

  def UrlEncodedHtmlForm[TELL,ASK, Html](
    parser: DataParser[ASK],
    html: HtmlForm[ASK, Html],
    renderTell: (TELL, String) => Html
  ): SimpleInteractionForm[FormUrlEncoded,TELL,ASK,Html] = {
    val underlying = new InputHtmlForm(parser, html, renderTell)
    underlying.transformIn(_.toInputTree)
  }

  protected[web] val required = "required"

}
