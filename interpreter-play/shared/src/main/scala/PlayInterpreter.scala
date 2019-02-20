package ltbs.uniform.interpreters.playframework

import cats.Monoid
import cats.data._
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all.{none => _, _}
import org.atnos.eff.syntax.all._
import play.api.data.Form
import ltbs.uniform._
import play.api._
import play.api.mvc._
import play.twirl.api.Html
import ltbs.uniform._
import ltbs.uniform.web._
import scala.concurrent.{ ExecutionContext, Future }

trait PlayInterpreter extends Compatibility.PlayController {

  def messages(request: Request[AnyContent]): Messages

  def renderForm(
    key: List[String],
    errors: ErrorTree,
    form: Html,
    breadcrumbs: List[String],
    request: Request[AnyContent],
    messages: Messages
  ): Html

  def listingPage[A : Htmlable](
    key: List[String],
    errors: ErrorTree,
    elements: List[A],
    messages: Messages
  ): Html

  implicit def convertMessages(implicit input: i18n.Messages): Messages = new Messages{
    override def apply(key: List[String],args: Any*): String = input(key, args)
    override def apply(key: String,args: Any*): String = input(key, args)
    def get(key: String,args: Any*): Option[String] = if (input.isDefinedAt(key))
      input.messages(key, args:_*).some
    else
      none[String]

    def get(key: List[String],args: Any*): Option[String] = key collectFirst {
      case k if input.isDefinedAt(k) => input.messages(k, args:_*)
    }

    def list(key: String,args: Any*): List[String] = {
      @annotation.tailrec
      def inner(cnt: Int = 2, acc: List[String] = Nil): List[String] =
        get(s"$key.$cnt", args:_*) match {
          case Some(m) => inner(cnt+1, m :: acc)
          case None    => acc
        }

      List(key, s"$key.1").map(get(_, args)).flatten ++ inner().reverse
    }
  }

  val log: Logger = Logger("uniform")

  def formToValidated[A](f: Form[A]): ValidatedData[A] =
    if (!f.hasErrors) f.value.map{_.valid}
    else Some(f.errors.head.message.invalid)

  type PlayStack = Fx.fx2[State[UniformCore, ?], Either[Result, ?]]

  type _core[Q]  = State[UniformCore,?] |= Q
  type _either[Q] = Either[Result,?] |= Q

  implicit class PlayEffectOps[STACK, A](e: Eff[STACK, A]) {

    def useForm[IN, OUT, NEWSTACK](
      wmFormC: PlayForm[IN,OUT]
    )(
      implicit member: Member.Aux[Uniform[IN,OUT,?], STACK, NEWSTACK],
      state: _core[NEWSTACK],
      eitherM: _either[NEWSTACK],
      request: Request[AnyContent],
      targetId: List[String]
    ): Eff[NEWSTACK, A] = useFormMap(_ => wmFormC)

    def useFormMap[IN, OUT, NEWSTACK](
      wmFormOUT: List[String] => PlayForm[IN,OUT]
    )(
      implicit member: Member.Aux[Uniform[IN,OUT,?], STACK, NEWSTACK],
      stateM: _core[NEWSTACK],
      eitherM: _either[NEWSTACK],
      request: Request[AnyContent],
      targetId: List[String]
    ): Eff[NEWSTACK, A] = e.translate(
      new Translate[Uniform[IN, OUT,?], NEWSTACK] {
        def apply[X](ax: Uniform[IN, OUT,X]): Eff[NEWSTACK, X] = {
          val wmForm: PlayForm[IN,X] = wmFormOUT(ax.key).imap(_.asInstanceOf[X])(_.asInstanceOf[OUT])

          val baseUrl = request.target.path.replaceFirst(targetId.mkString("/") + "$", "")

          def breadcrumbsToUrl(in: List[List[String]]): List[String] =
            in.map { xs => baseUrl + xs.mkString("/") }

          ax match {
            case Uniform(id, tell, default, validation) =>

              for {
                g <- core
                method = request.method.toLowerCase
                UniformCore(state, breadcrumbs, _) = g
                dbObject: Option[OUT] = {
                  val o = state.get(id).flatMap(wmFormOUT(id).decode(_).flatMap(validation(_).toEither) match {
                    case Left(e) =>
                      log.warn(s"$id - serialised data present, but failed validation - $e")
                      None
                    case Right(r) => Some(r)
                  })
                  o
                }

                ret <- (method, dbObject, targetId) match {
                  case ("get", None, `id`) =>
                    log.info(s"$id - nothing in database, step in URI, render empty form")
                    left[NEWSTACK, Result, X](Ok(renderForm(id, Tree.empty,
                      wmForm.render(id.last, tell, None, request),
                      breadcrumbsToUrl(breadcrumbs), request, messages(request)
                    )))

                  case ("get", Some(o), `id`) =>
                    val encoded = wmForm.encode(o.asInstanceOf[X]) // FormUrlEncoded.readString(wmForm.encode(o)).prefix(id).writeString
                    log.info(s"""|$id - something in database, step in URI, user revisiting old page, render filled in form
                                 |\t\t data: $o
                                 |\t\t encoded: $encoded """.stripMargin)
                    left[NEWSTACK, Result, X](Ok(
                      renderForm(id, Tree.empty,
                      wmForm.render(id.last, tell, Some(encoded), request),
                      breadcrumbsToUrl(breadcrumbs), request, messages(request)
                    )))

                  case ("get", Some(data), _) =>
                    log.info(s"$id - something in database, not step in URI, pass through")
                    crumbPush(id) >>
                    Eff.pure[NEWSTACK, X](data.asInstanceOf[X])

                  case ("post", _, `id`) =>
                    val data: Encoded =
                      wmForm.receiveInput(request)

                    def validationToErrorTree[V](f: V => Validated[String,V]): V => Either[ErrorTree,V] = {
                      x => f(x).toEither.leftMap(Tree(_))
                    }

                    def v(in: X): Either[ErrorTree, X] = {
                      validationToErrorTree(validation)(in.asInstanceOf[OUT]).map{_.asInstanceOf[X]}
                    }

                    wmForm.decode(data).flatMap(v) match {
                      case Left(errors) =>
                        log.info(s"$id - form submitted, step in URI, validation failure")
                        log.info(s"  errors: $errors")
                        log.info(s"  data: $data")
                        left[NEWSTACK, Result, X](BadRequest(renderForm(id, errors,
                          wmForm.render(id.last, tell, Some(data), request, errors),
                          breadcrumbsToUrl(breadcrumbs), request, messages(request)
                        )))
                      case Right(o) =>
                        log.info(s"$id - form submitted, step in URI, validation pass")
                        (db.encoded(id) = wmForm.encode(o)) >>
                        crumbPush(id) >>
                        Eff.pure[NEWSTACK, X](o)

                    }

                  case ("post", Some(_), _) if breadcrumbs.contains(targetId) =>
                    log.info(s"$id - something in database, previous page submitted")
                    crumbPush(id) >>
                    left[NEWSTACK, Result, X](Redirect(s"${baseUrl}${id.mkString("/")}"))

                  case ("post", Some(data), _) =>
                    log.info(s"$id - something in database, posting, not step in URI nor previous page -> pass through")
                    crumbPush(id) >>
                      Eff.pure[NEWSTACK,X](data.asInstanceOf[X])

                  case ("post", _, _) | ("get", _, _) =>
                    log.warn(
                      s"""|$id - nothing else seems applicable. maybe this should be a 404?
                          |\t\t method:$method
                          |\t\t dbObject:$dbObject
                          |\t\t targetId:$targetId""".stripMargin)

                    left[NEWSTACK, Result, X](Redirect(s"${baseUrl}${id.mkString("/")}"))
                }
              } yield ret
          }
        }
      }
    )

    def delist[OUT, NEWSTACK, INNER: _uniformCore](
      subJourneyP: (List[OUT], Option[OUT]) => Eff[INNER, OUT]
    )(
      implicit member: Member.Aux[UniformAsk[List[OUT],?], STACK, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      listingPage: _uniform[List[OUT], ListControl, NEWSTACK],
      confirmationPage: _uniform[OUT, Boolean, INNER],
      parser: DataParser[List[OUT]],
      f: IntoPoly[INNER,NEWSTACK]
    ): Eff[NEWSTACK,A] = {

      def w(allElements: List[OUT], removalCandidate: OUT): Eff[INNER, Boolean] =
        dialogue[OUT,Boolean]("confirm")(removalCandidate).in[INNER]

      delistWithCustomRemove[OUT, NEWSTACK, INNER](
        subJourneyP,
        w
      )
    }

    def delistWithCustomRemove[OUT, NEWSTACK, INNER](
      subJourneyP: (List[OUT], Option[OUT]) => Eff[INNER, OUT],
      removeConfirmation: (List[OUT], OUT) => Eff[INNER, Boolean]
    )(
      implicit member: Member.Aux[UniformAsk[List[OUT],?], STACK, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      listingPage: _uniform[List[OUT], ListControl, NEWSTACK],
      parser: DataParser[List[OUT]],
      f: IntoPoly[INNER,NEWSTACK]
    ): Eff[NEWSTACK,A] = e.translate(
      new Translate[UniformAsk[List[OUT],?], NEWSTACK] {

        def serialise(in: List[OUT]): String = FormUrlEncoded.fromInputTree(parser.unbind(in)).writeString
        def deserialise(in: String): List[OUT] = parser.bind(FormUrlEncoded.readString(in).toInputTree) match {
          case Left(t) if t.isEmpty => Nil
          case Left(e) => throw new IllegalStateException(e.toString)
          case Right(r) => r
        }

        def apply[X](ax: UniformAsk[List[OUT],X]): Eff[NEWSTACK, X] = {

          def real: Eff[NEWSTACK,List[OUT]] = ax match {
            case Uniform(id, tell, default, validation) =>

              def read: Eff[NEWSTACK, Option[List[OUT]]] =
                db.encoded.get(id :+ "__data").map(_.map(deserialise))

              def write(in: List[OUT]): Eff[NEWSTACK, Unit] =
                db.encoded(id :+ "__data") = serialise(in)

              def process(elements: List[OUT]): Eff[NEWSTACK,List[OUT]] = {
                uniformP[List[OUT],ListControl,NEWSTACK](id, elements) >>=
                {_ match {
                  case ltbs.uniform.web.Continue =>
                    Eff.pure[NEWSTACK,List[OUT]](elements)

                  case AddAnother =>
                    subjourney("add") {
                      subJourneyP(elements, None).into[NEWSTACK]
                    } >>= {x =>
                      db.remove(id) >>
                      db.removeRecursive(id.dropRight(1) :+ "add") >>
                      write(elements :+ x) >>
                      process(elements :+ x)}

                  case Edit(ordinal) =>
                    subjourney("edit") {
                      subJourneyP(elements, elements.get(ordinal)).into[NEWSTACK]
                    } >>= {x =>
                      db.remove(id) >>
                      db.removeRecursive(id.dropRight(1) :+ "edit") >>
                      write(elements.replace(ordinal, x)) >>
                      process(elements.replace(ordinal, x))}

                  case Delete(ordinal) =>
                    subjourney("delete") {
                      removeConfirmation(elements, elements(ordinal)).into[NEWSTACK]
                    } >>= {
                      if (_) {
                        write(elements.delete(ordinal)) >>
                        db.removeRecursive(id.dropRight(1) :+ "delete") >>
                        db.remove(id) >>
                        process(elements.delete(ordinal))
                      } else
                        db.removeRecursive(id.dropRight(1) :+ "delete") >>
                        db.remove(id) >>
                        process(elements)

                    }
                }}
              }

              read >>= { state => process(state.orElse(default).getOrElse(Nil)) }

          }
          real.map{_.asInstanceOf[X]}
        }
      }
    )

  //   def delist[OUT, NEWSTACK, INNER](
  //     subJourneyP: (List[OUT], Option[OUT]) => Eff[INNER, OUT]
  //   )(
  //     implicit member: Member.Aux[UniformAsk[List[OUT],?], STACK, NEWSTACK],
  //     stateM: _core[NEWSTACK],
  //     eitherM: _either[NEWSTACK],
  //     listingPage: _uniform[List[OUT], ListControl, NEWSTACK],
  //     parser: DataParser[List[OUT]],
  //     f: IntoPoly[INNER,NEWSTACK]
  //   ): Eff[NEWSTACK,A] = e.translate(
  //     new Translate[UniformAsk[List[OUT],?], NEWSTACK] {

  //       val removeConfirmation: (List[String], List[OUT], OUT) => Eff[NEWSTACK, Boolean] = {alwaysYes[NEWSTACK, OUT] _}

  //       def serialise(in: List[OUT]): String = FormUrlEncoded.fromInputTree(parser.unbind(in)).writeString
  //       def deserialise(in: String): List[OUT] = parser.bind(FormUrlEncoded.readString(in).toInputTree) match {
  //         case Left(t) if t.isEmpty => Nil
  //         case Left(e) => throw new IllegalStateException(e.toString)
  //         case Right(r) => r
  //       }

  //       def apply[X](ax: UniformAsk[List[OUT],X]): Eff[NEWSTACK, X] = {

  //         def real: Eff[NEWSTACK,List[OUT]] = ax match {
  //           case Uniform(id, tell, default, validation) =>

  //             def read: Eff[NEWSTACK, Option[List[OUT]]] =
  //               db.encoded.get(id :+ "__data").map(_.map(deserialise))

  //             def write(in: List[OUT]): Eff[NEWSTACK, Unit] =
  //               db.encoded(id :+ "__data") = serialise(in)

  //             def process(elements: List[OUT]): Eff[NEWSTACK,List[OUT]] = {
  //               uniformP[List[OUT],ListControl,NEWSTACK](id, elements) >>=
  //               {_ match {
  //                 case ltbs.uniform.web.Continue => Eff.pure[NEWSTACK,List[OUT]](elements)

  //                 case AddAnother =>
  //                   subjourney("add") {
  //                     subJourneyP(elements, None).into[NEWSTACK]
  //                   } >>= {x =>
  //                     db.remove(id) >>
  //                     db.removeRecursive(id.dropRight(1) :+ "add") >>
  //                     write(elements :+ x) >>
  //                     process(elements :+ x)}

  //                 case Edit(ordinal) =>
  //                   subjourney("edit") {
  //                     subJourneyP(elements, elements.get(ordinal)).into[NEWSTACK]
  //                   } >>= {x =>
  //                     db.remove(id) >>
  //                     db.removeRecursive(id.dropRight(1) :+ "edit") >>
  //                     write(elements.replace(ordinal, x)) >>
  //                     process(elements.replace(ordinal, x))}

  //                 case Delete(ordinal) =>
  //                   removeConfirmation(id, elements, elements(ordinal)) >>= {
  //                     if (_) {
  //                       db.remove(id) >>
  //                       write(elements.delete(ordinal)) >>
  //                       left[NEWSTACK, Result, List[OUT]](Redirect(".."))
  //                     } else
  //                       left[NEWSTACK, Result, List[OUT]](Redirect(".."))
  //                   }
  //               }}
  //             }

  //             read >>= { state => process(state.orElse(default).getOrElse(Nil)) }

  //         }
  //         real.map{_.asInstanceOf[X]}
  //       }
  //     }
  //   )

  }

  implicit val scheduler = ExecutorServices.schedulerFromGlobalExecutionContext

  def runWeb[A](
    program: Eff[PlayStack, A],
    persistence: Persistence,
    purgeJourneyOnCompletion: Boolean = true
  )(
    terminalFold: A => Future[Result]
  )(implicit ec: ExecutionContext): Future[Result] =
    persistence.dataGet.map {
      data => program
        .runEither
        .runState(UniformCore(state = data))
        .run
    } >>= {
      _ match {
        case (Left(result), UniformCore(db, _, _)) =>
          persistence.dataPut(db).map(_ => result)
        case (Right(a), UniformCore(db, _, _)) =>
          val newDb: DB = if (purgeJourneyOnCompletion) (Monoid[DB].empty) else db
          persistence.dataPut(newDb) >> terminalFold(a)
      }
    }

  def listingTable[E](csrf: Html)(
    key: String,
    render: (String, List[(Html, Option[Html], Option[Html])], Int, Int, Messages) => Html,
    elementToHtml: E => Html,
    messages: Messages
  )(elements: List[E]): Html = {

    def edit(i: Int) = Html(
      s"""|<form action="$key" method="post"> $csrf
          |  <input type="hidden" name="$key.Edit.ordinal" value="$i" />
          |    <button type="submit" name="$key" value="Edit" class="link-button">
          |      Edit
          |    </button>
          |</form>
          |""".stripMargin
    )

    def delete(i: Int) = Html(
      s"""|<form action="$key" method="post"> $csrf
          |  <input type="hidden" name="$key.Delete.ordinal" value="$i" />
          |    <button type="submit" name="$key" value="Delete" class="link-button">
          |      Delete
          |    </button>
          |</form>
          |""".stripMargin
    )

    render(key, elements.zipWithIndex.map{
      case (x,i) => (elementToHtml(x), Some(edit(i)), Some(delete(i)))
    }, 0, Int.MaxValue, messages)

  }
}
