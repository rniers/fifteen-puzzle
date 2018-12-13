package puzzle

import puzzle.PlayerMove._


case class PuzzleState private(board: List[Int], dim: Int) {

  /**
    * Convert coordinates of an entry to index in flat list
    * @param x - col number starting form 0
    * @param y - raw number starting from 0
    * @return
    */
  @inline private def toIdx(x: Int, y: Int): Int = y * dim + x

  /**
    * Detect if supplied player move can be applied to current
    * puzzle board state
    *
    * @param move - ADT encoding player move
    * @return
    */
  private def isAllowed(move: PlayerMove): Boolean = {
    val (x, y) = coordinates

    move match {
      case Left => x > 0
      case Right => x < (dim - 1)
      case Down => y < (dim -1)
      case Up => y > 0
    }
  }

  /**
    * Current coordinate of empty position
    *
    * @return - tuple of (x, y) coordinates, enumeration starts with 0
    */
  def coordinates: (Int, Int) = {
    val idx = board.indexOf(0)
    (idx % dim, idx / dim)
  }

  /**
    * Apply player move to current puzzle board state
    *
    * @param action player input action
    * @return
    */
  def takeMove(action: PlayerMove): PuzzleState = {
    if (!isAllowed(action)) this
    else {

      val (x, y) = coordinates
      val (newX, newY) = action match {
        case Left => (x - 1, y)
        case Right => (x + 1, y)
        case Down => (x, y + 1)
        case Up => (x, y - 1)
      }
      // swap values
      val idx = toIdx(newX, newY) // index of moving element
      val tmp = board(idx)

      this.copy(board = board.updated(idx, 0).updated(toIdx(x, y), tmp))
    }
  }

  /**
    * Return list of player actions allowed in current game state
    *
    * @return - list of possible moves
    */
  lazy val allowedMoves: List[PlayerMove] = {
    Left :: Right :: Up :: Down :: Nil filter isAllowed
  }

  /**
    * Return internal representation of game board
    *
    * @return
    */
  lazy val show: List[List[Int]] = {
    board.grouped(dim).toList
  }

  /**
    * Determine if current state is solved
    *
    * @return
    */
  lazy val isSolved: Boolean = {
    val view = board.take(dim * dim - 1)
    view.sliding(2).forall {
      case x :: y :: Nil => x < y
      case _ => false
    }
  }
}

object PuzzleState {

  val MinMoves = 80
  val MaxMoves = 100

  /**
    * Shuffle puzzle state to create solvable permutation
    *
    * @param state - puzzle state to shuffle
    * @return
    */
  private def shuffle(state: PuzzleState): PuzzleState = {

    val rand = new scala.util.Random()
    val moves = MinMoves + rand.nextInt(MaxMoves - MinMoves)

    (1 to moves).foldLeft(state) { (state, _) =>
      // chose random move
      val move = state.allowedMoves(rand.nextInt(state.allowedMoves.length))
      state.takeMove(move)
    }
  }

  /**
    * Construct random puzzle state
    *
    * @param dim - size of the gird
    * @return
    */
  def apply(dim: Int = 4): PuzzleState = {
    val l = (1 until dim * dim).toList :+ 0
    shuffle(PuzzleState(l, dim))
  }

  /**
    * Build puzzle from list of integers
    *
    * @param board - flat representation of the puzzle board
    * @return
    */
  def apply(board: List[Int]): Option[PuzzleState] = {
    val root = math.sqrt(board.length)

    if ((root - math.floor(root)) == 0) Some(new PuzzleState(board, root.toInt))
    else None
  }
}