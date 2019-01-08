package ltbs.uniform

import cats.data.Validated
import cats.implicits._

/** Represents asking the user for a single value, although this value
  * could be a list (for example). 
  * 
  * In practice this accounts for almost all user interactions.
  */
// case class UniformAsk[STACK, A](
//   key: String,
//   validation: A => Validated[String,A] = {a:A => a.valid}
// )

case class UniformInteraction[STACK, TELL, ASK](
  key: String,
  value: TELL, 
  validation: ASK => Validated[String,ASK] = {a:ASK => a.valid}
)
