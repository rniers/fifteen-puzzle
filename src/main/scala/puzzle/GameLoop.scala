package puzzle

import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import org.jline.terminal.TerminalBuilder
import cats.syntax.all._


object GameLoop extends IOApp {

  val input: Stream[IO, PlayerMove] = {
    val terminal = TerminalBuilder.builder().system(true).build()
    terminal.enterRawMode()

    val reader = terminal.reader()

    Stream
      .repeatEval(IO(reader.read()))
      .collect {
        case 'w' => PlayerMove.Up
        case 's' => PlayerMove.Down
        case 'a' => PlayerMove.Left
        case 'd' => PlayerMove.Right
      }
  }


  val gameLoop: Stream[IO, PuzzleState] = {

    val initial = PuzzleState()
    input
      .scan(initial)( (s, m) => s.takeMove(m) )
      .takeWhile(!_.isSolved)
  }

  override def run(args: List[String]): IO[ExitCode] = {

    UI.invitation *>
    gameLoop.flatMap(state => Stream.eval(UI.display(state))).compile.drain *>
    UI.finish *>
    IO.pure(ExitCode.Success)
  }

}

