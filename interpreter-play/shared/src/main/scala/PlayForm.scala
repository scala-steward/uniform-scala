package ltbs.uniform.interpreters.playframework

import ltbs.uniform.common.web._
import ltbs.uniform.datapipeline._

object PlayForm {
  def automatic[T,A](
    parser: DataParser[A],
    html: HtmlForm[T,A],
    messages: Messages
  ): PlayForm[T,A] = 
    sifProfunctor.lmap(
      UrlEncodedHtmlForm[T,A](parser, html, messages)
    )(_.body.asFormUrlEncoded.getOrElse(Map.empty))

}
