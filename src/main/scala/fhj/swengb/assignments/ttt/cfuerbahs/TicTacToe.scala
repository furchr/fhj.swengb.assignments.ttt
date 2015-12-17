package fhj.swengb.assignments.ttt.cfuerbahs

import scala.collection.Set

/**
  * models the different moves the game allows
  *
  * each move is made by either player a or player b.
  */
sealed trait TMove {
  def idx: Int
}

case object TopLeft extends TMove {
  override def idx: Int = 0
}

case object TopCenter extends TMove {
  override def idx: Int = 1
}

case object TopRight extends TMove {
  override def idx: Int = 2
}

case object MiddleLeft extends TMove {
  override def idx: Int = 3
}

case object MiddleCenter extends TMove {
  override def idx: Int = 4
}

case object MiddleRight extends TMove {
  override def idx: Int = 5
}

case object BottomLeft extends TMove {
  override def idx: Int = 6
}

case object BottomCenter extends TMove {
  override def idx: Int = 7
}

case object BottomRight extends TMove {
  override def idx: Int = 8
}


/**
  * for a tic tac toe game, there are two players, player A and player B
  */
sealed trait Player

case object PlayerA extends Player

case object PlayerB extends Player

object TicTacToe {


  //furchr

  def main(args: Array[String]) {

    val t = TicTacToe().turn(BottomRight, PlayerA).turn(BottomCenter,PlayerB).turn(BottomLeft, PlayerA).turn(MiddleCenter,PlayerA).turn(MiddleRight,PlayerB)
      .turn(MiddleLeft,PlayerB).turn(TopCenter,PlayerA).turn(TopRight,PlayerB).turn(TopLeft,PlayerB)

    //test output
    print(t.asString())
    println("RemainingMoves: " + t.remainingMoves)
    println("Number of remaining moves: " + t.remainingMoves.size)

    //test remainingmoves
    println("Is the game over? " + t.gameOver)
    println("Winner is: " + t.winner.getOrElse(None))

  }

  //furchr





  /**
    * creates an empty tic tac toe game
    * @return
    */
  def apply(): TicTacToe = TicTacToe(Map())//furchr


  /**
    * For a given tic tac toe game, this function applies all moves to the game.
    * The first element of the sequence is also the first move.
    *
    * @param t
    * @param moves
    * @return
    */
  def play(t: TicTacToe, moves: Seq[TMove]): TicTacToe = ???

  /**
    * creates all possible games.
    * @return
    */
  def mkGames(): Map[Seq[TMove], TicTacToe] = ???

  //furchr
  val allGames : Map[Seq[TMove], TicTacToe] = Map()
  //return allGames
  //furchr

}

/**
  * Models the well known tic tac toe game.
  *
  * The map holds the information which player controls which field.
  *
  * The nextplayer parameter defines which player makes the next move.
  */
case class TicTacToe(moveHistory: Map[TMove, Player],
                     nextPlayer: Player = PlayerA) {

  /**
    * outputs a representation of the tic tac toe like this:
    *
    * |---|---|---|
    * | x | o | x |
    * |---|---|---|
    * | o | x | x |
    * |---|---|---|
    * | x | o | o |
    * |---|---|---|
    *
    *
    * @return
    */
  def asString(): String =
  //furchr
  {

    val indexMap = Map(0 -> 16, 1 -> 20, 2 -> 24,
      3 -> 44, 4 -> 48, 5 -> 52,
      6 -> 72, 7 -> 76, 8 -> 80)

    var board: String =
      "|---|---|---|\n" +
        "|   |   |   |\n" +
        "|---|---|---|\n" +
        "|   |   |   |\n" +
        "|---|---|---|\n" +
        "|   |   |   |\n" +
        "|---|---|---|\n"


    for ((k, v) <- moveHistory) {
      if (v == PlayerA) {
        board = board.updated(indexMap(k.idx), "O").mkString
      }
      else if (v == PlayerB) {
        board = board.updated(indexMap(k.idx), "X").mkString
      }
      else {
        board = board.updated(indexMap(k.idx), " ").mkString
      }
    }
    board
  }

  /**
    * is true if the game is over.
    *
    * The game is over if either of a player wins or there is a draw.
    */
  val winningScenarios: List[Set[TMove]] = List(Set(TopLeft, TopCenter, TopRight), Set(MiddleLeft, MiddleCenter, MiddleRight), Set(BottomLeft, BottomCenter, BottomRight),
    Set(TopLeft, MiddleCenter, BottomRight), Set(TopRight, MiddleCenter, BottomLeft), Set(TopCenter, MiddleCenter, BottomCenter), Set(TopLeft, MiddleLeft, BottomLeft),
    Set(TopRight, MiddleRight, BottomRight))

  def checkIfWon(player: Player): Boolean = {

    if(winningScenarios.head.subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if(winningScenarios(1).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if(winningScenarios(2).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if(winningScenarios(3).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if(winningScenarios(4).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if(winningScenarios(5).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if(winningScenarios(6).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if(winningScenarios(7).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else
      false
  }


  /**
    * the moves which are still to be played on this tic tac toe.
    */
  val tMoves: Set[TMove] = Set(TopLeft, TopCenter, TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)
  val fieldsNotSet = moveHistory.filter(x => x._2 != PlayerA || x._2 != PlayerB).keySet

  //furchr

  /**
    * is true if the game is over.
    *
    * The game is over if either of a player wins or there is a draw.
    */
  val gameOver : Boolean = true match {//furchr
    case gameEnd => winner.isDefined || remainingMoves.isEmpty
    case _ => false
  }
  //furchr

  /**
    * the moves which are still to be played on this tic tac toe.
    */
  val remainingMoves: Set[TMove] = tMoves.diff(fieldsNotSet)

  /**
    * given a tic tac toe game, this function returns all
    * games which can be derived by making the next turn. that means one of the
    * possible turns is taken and added to the set.
    */
  lazy val nextGames: Set[TicTacToe] = ???

  /**
    * Either there is no winner, or PlayerA or PlayerB won the game.
    *
    * The set of moves contains all moves which contributed to the result.
    */
  def winner: Option[(Player, Set[TMove])] = {
    //furchr
    if(checkIfWon(PlayerA))
      Some(PlayerA,moveHistory.filter(_._2 == PlayerA).keySet)
    else if(checkIfWon(PlayerB))
      Some(PlayerB,moveHistory.filter(_._2 == PlayerB).keySet)
    else None
    //furchr
  }

  /**
    * returns a copy of the current game, but with the move applied to the tic tac toe game.
    *
    * @param move to be played
    * @param player the player
    * @return
    */
  def turn(p: TMove, player: Player): TicTacToe = {//furchr

    if (!moveHistory.get(p).contains(PlayerA) || !moveHistory.get(p).contains(PlayerB)) {
      if (player.equals(PlayerA))
        TicTacToe(this.moveHistory + (p -> player), PlayerB)
      else
        TicTacToe(this.moveHistory + (p -> player), PlayerA)

    }
    else{
      TicTacToe(this.moveHistory, nextPlayer)
    }
    //furchr
  }

}


