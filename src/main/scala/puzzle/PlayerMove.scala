package puzzle


sealed trait PlayerMove
object PlayerMove {
  case object Left extends PlayerMove
  case object Right extends PlayerMove
  case object Up extends PlayerMove
  case object Down extends PlayerMove
}