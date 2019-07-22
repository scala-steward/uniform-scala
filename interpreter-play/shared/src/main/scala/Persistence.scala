package ltbs.uniform
package interpreters.playframework

import cats.implicits._
import play.api._,mvc._
import concurrent.{Future,ExecutionContext}
import java.util.UUID
import cats.data.{EitherT, RWST}

case class LowLevelDbAccess()(implicit ec: ExecutionContext) {

  def writeF[A](f: DB => DB): WebMonad[Unit] =
    EitherT[WebInner, Result, Unit] {
      RWST { case ((_, _, _), (path, db)) ⇒
        Future{ ((),(path,f(db)),().asRight[Result]) }
      }
    }

  def readF[A](f: DB => A): WebMonad[A] =
    EitherT[WebInner, Result, A] {
      RWST { case ((_, _, _), (path, db)) ⇒
        Future{ ((),(path,db),f(db).asRight[Result]) }
      }
    }

  def readAll: WebMonad[DB] = readF(identity)

  def writeAll(in: DB): WebMonad[Unit]  = writeF(_ => in)

  def read(key: List[String]): WebMonad[Option[String]] =
    readF(_.get(key))

  def write(key: List[String], data: String): WebMonad[Unit] =
    writeF(_ + (key -> data))

  def erase(key: List[String]): WebMonad[Unit] =
    writeF(_ - key)

  def eraseRecursive(key: List[String]): WebMonad[Unit] =
    writeF(_.filterKeys(_.startsWith(key)))

  def get = read _
  def update = write _ 
}

trait PersistenceEngine {
  def apply(request: Request[AnyContent])(f:
      DB => Future[(DB,Result)]): Future[Result]
}

case class DebugPersistence(underlying: UUIDPersistence)(implicit ec: ExecutionContext) extends UUIDPersistence {

  val log: Logger = Logger("persistence")  

  def load(uuid: UUID): Future[DB] = 
    underlying.load(uuid).map{ r => 
      log.info(s"load($uuid): ${r.toString}")
      r
    }

  def save(uuid: UUID, db: DB): Future[Unit] = {
    underlying.save(uuid, db).map{ case _ => 
      log.info(s"save($uuid, ${db.toString})")
    }
  }
}

abstract class UUIDPersistence()(implicit ec: ExecutionContext) extends PersistenceEngine {
  def load(uuid: UUID): Future[DB]
  def save(uuid: UUID, db: DB): Future[Unit]
  def apply(request: Request[AnyContent])(f: DB ⇒ Future[(DB,Result)]): Future[Result] = {

    val uuid: UUID = request.session.get("uuid").map{UUID.fromString}
      .getOrElse( UUID.randomUUID )

    for {
      db              ← load(uuid)
      (newDb, result) ← f(db)
      _               ← save(uuid, newDb)
    } yield result.withSession(
      request.session + ("uuid" → uuid.toString)
    )
  }
}

final case class UnsafePersistence(
  var state: Map[UUID,DB] = Map.empty
)(implicit ec: ExecutionContext) extends UUIDPersistence()(ec) {

  def load(uuid: UUID): Future[DB] =
    Future.successful(state.withDefaultValue(Map.empty)(uuid))

  def save(uuid: UUID,db: DB): Future[Unit] = {
    state = state + (uuid -> db)
    Future.successful(())
  }
}
