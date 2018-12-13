package puzzle

import org.scalatest._
import puzzle.PlayerMove.{Up, _}

class PuzzleStateSpec extends FlatSpec with Matchers {

  "A PuzzleState" should "create board" in {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0)
    val state = PuzzleState(l).get

    state.show shouldEqual List(
      List( 1, 2, 3, 4),
      List(5, 6, 7, 8),
      List(9, 10, 11, 12),
      List(13, 14, 15, 0)
    )
  }

  "A PuzzleState.coordinates" should "correctly detect hole position" in {
    val state = List(
      1, 2, 3, 4,
      5, 6, 0, 7,
      8, 9, 10, 11,
      12, 13, 14, 15
    )

    PuzzleState(state).get.coordinates shouldBe (2, 1)
  }

  it should "be correct for bottom right corner" in {
    val state = List(
      1, 2, 3, 4,
      5, 6, 15, 7,
      8, 9, 10, 11,
      12, 13, 14, 0
    )

    PuzzleState(state).get.coordinates shouldBe (3, 3)
  }

  it should "be correct for bottom left corner" in {
    val state = List(
      1, 2, 3, 4,
      5, 6, 12, 7,
      8, 9, 10, 11,
      0, 13, 14, 15
    )

    PuzzleState(state).get.coordinates shouldBe (0, 3)
  }

  it should "be correct for upper left corner" in {
    val state = List(
      0, 2, 3, 4,
      5, 6, 1, 7,
      8, 9, 10, 11,
      12, 13, 14, 15
    )

    PuzzleState(state).get.coordinates shouldBe (0, 0)
  }

  it should "be correct for upper right corner" in {
    val state = List(
      1, 2, 3, 0,
      5, 6, 4, 7,
      8, 9, 10, 11,
      12, 13, 14, 15
    )

    PuzzleState(state).get.coordinates shouldBe (3, 0)
  }

  "A PuzzleState.allowedActions" should "allow all moves for central positions" in {

    val state = List(
      1, 2, 3, 4,
      5, 6, 0, 7,
      8, 9, 10, 11,
      12, 13, 14, 15
    )

    val allowedMoves = PuzzleState(state).get.allowedMoves

    allowedMoves should contain (Up)
    allowedMoves should contain (Down)
    allowedMoves should contain (Left)
    allowedMoves should contain (Right)
  }

  it should "allow only left and down for right upper corner" in {
    val state = List(
      1, 2, 3, 0,
      5, 6, 4, 7,
      8, 9, 10, 11,
      12, 13, 14, 15
    )

    val allowedMoves = PuzzleState(state).get.allowedMoves

    allowedMoves.size shouldBe 2
    allowedMoves should contain (Down)
    allowedMoves should contain (Left)
  }

  it should "allow only left and up for right bottom corner" in {
    val state = List(
      1, 2, 3, 15,
      5, 6, 4, 7,
      8, 9, 10, 11,
      12, 13, 14, 0
    )

    val allowedMoves = PuzzleState(state).get.allowedMoves

    allowedMoves.size shouldBe 2
    allowedMoves should contain (Up)
    allowedMoves should contain (Left)
  }

  it should "allow only right and up for left bottom corner" in {
    val state = List(
      1, 2, 3, 15,
      5, 6, 4, 7,
      8, 9, 10, 11,
      0, 12, 13, 14
    )

    val allowedMoves = PuzzleState(state).get.allowedMoves

    allowedMoves.size shouldBe 2
    allowedMoves should contain (Up)
    allowedMoves should contain (Right)
  }

  it should "allow only right and down for left upper corner" in {
    val state = List(
      0, 1, 2, 3,
      4, 5, 6, 7,
      8, 9, 10, 11,
      12, 13, 14, 15
    )

    val allowedMoves = PuzzleState(state).get.allowedMoves

    allowedMoves.size shouldBe 2
    allowedMoves should contain (Down)
    allowedMoves should contain (Right)
  }

  it should "allow right, left and down for middle position in top line" in {
    val state = List(
      1, 0, 2, 3,
      4, 5, 6, 7,
      8, 9, 10, 11,
      12, 13, 14, 15
    )

    val allowedMoves = PuzzleState(state).get.allowedMoves

    allowedMoves.size shouldBe 3
    allowedMoves should contain (Down)
    allowedMoves should contain (Right)
    allowedMoves should contain (Left)
  }


  "A PuzzleState.isSolved" should "determine winning layout" in {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0)
    val state =PuzzleState(l).get

    state.isSolved shouldBe true
  }

  it should "ignore other layouts" in {
    val state =  PuzzleState(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0, 15)).get
    val state2 = PuzzleState(List(1, 2, 3, 5, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0)).get

    state.isSolved shouldBe false
    state2.isSolved shouldBe false
  }


  "A PuzzleState.takeAction" should "move empty position" in {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0, 15)
    val state = PuzzleState(l).get

    state.takeMove(Left).show.flatten shouldEqual List(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 0, 14, 15
    )

    state.takeMove(Up).show.flatten shouldEqual List(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 0, 12,
      13, 14, 11, 15
    )

    state.takeMove(Up).takeMove(Up).takeMove(Down).show.flatten shouldEqual List(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 0, 12,
      13, 14, 11, 15
    )

    state.takeMove(Right).show.flatten shouldEqual List(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 0
    )
  }
}
