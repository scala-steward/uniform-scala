package ltbs.uniform.interpreters.playframework

import play.twirl.api.Html

object PlayTwirl extends PlayWriteable[Html]
trait PlayInterpreter extends PlayTwirl.Interpreter

