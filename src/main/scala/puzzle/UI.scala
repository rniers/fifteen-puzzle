package puzzle

import cats.effect.IO


object UI {

  val invitation = IO(println(
    """
      |Puzzle game.
      |To move numbers position use (W,A,S,D) keywords.
    """.stripMargin
  ))

  val finish = IO(println("You won."))

  def display(puzzleState: PuzzleState): IO[Unit] = IO {

    println(s"Current state: solved - ${puzzleState.isSolved}\n")

    def printRow(r: List[Int]): String = {
      r.foldLeft("") { (acc, n) =>
        if (n == 0) acc + "    "
        else acc + n.toString.padTo(4, " ").foldLeft("")(_ + _)
      } + "\n"
    }

    val view = puzzleState.show.map(printRow).reduce(_ + _)
    println(view)
  }

}