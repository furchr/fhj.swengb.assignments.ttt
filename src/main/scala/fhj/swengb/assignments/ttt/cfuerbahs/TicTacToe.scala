//tictactoe 2015-12-17_2124
package fhj.swengb.assignments.ttt.cfuerbahs;import scala.collection.Set;
sealed trait TMove {def idx: Int};sealed trait Player;
case object TopLeft extends TMove {override def idx: Int = 0}
case object TopCenter extends TMove {override def idx: Int = 1}
case object TopRight extends TMove {override def idx: Int = 2}
case object MiddleLeft extends TMove {override def idx: Int = 3}
case object MiddleCenter extends TMove {override def idx: Int = 4}
case object MiddleRight extends TMove {override def idx: Int = 5}
case object BottomLeft extends TMove {override def idx: Int = 6}
case object BottomCenter extends TMove {override def idx: Int = 7}
case object BottomRight extends TMove {override def idx: Int = 8}
case object PlayerA extends Player;case object PlayerB extends Player;

object TicTacToe {def main(args: Array[String]) {val t = TicTacToe().turn(BottomRight, PlayerA).turn(BottomCenter, PlayerB).turn(BottomLeft, PlayerA).turn(MiddleCenter, PlayerA).turn(MiddleRight, PlayerB)}
  def apply(): TicTacToe = TicTacToe(Map()); def play(t: TicTacToe, moves: Seq[TMove]): TicTacToe = ???; def mkGames(): Map[Seq[TMove], TicTacToe] = ???; val allGames: Map[Seq[TMove], TicTacToe] = Map()}

case class TicTacToe(moveLog: Map[TMove, Player], nextPlayer: Player = PlayerA) {
  def convert2String(): String = {val indexMap = Map(0 -> 16, 1 -> 20, 2 -> 24, 3 -> 44, 4 -> 48, 5 -> 52, 6 -> 72, 7 -> 76, 8 -> 80)
    var board: String = "|---|---|---|\n" + "|   |   |   |\n" + "|---|---|---|\n" + "|   |   |   |\n" + "|---|---|---|\n" + "|   |   |   |\n" + "|---|---|---|\n"
    for ((k, v) <- moveLog) {if (v == PlayerA) {board = board.updated(indexMap(k.idx), "O").mkString} else if (v == PlayerB) {board = board.updated(indexMap(k.idx), "X").mkString} else {board = board.updated(indexMap(k.idx), " ").mkString}};board}

  val win: List[Set[TMove]] = List(Set(TopLeft, TopCenter, TopRight), Set(MiddleLeft, MiddleCenter, MiddleRight), Set(BottomLeft, BottomCenter, BottomRight), Set(TopLeft, MiddleCenter, BottomRight), Set(TopRight, MiddleCenter, BottomLeft), Set(TopCenter, MiddleCenter, BottomCenter), Set(TopLeft, MiddleLeft, BottomLeft), Set(TopRight, MiddleRight, BottomRight))

  def checkWon(player: Player): Boolean = {
    if (win.head.subsetOf(moveLog.filter(_._2 == player).keySet)) true
    else if (win(1).subsetOf(moveLog.filter(_._2 == player).keySet)) true
    else if (win(2).subsetOf(moveLog.filter(_._2 == player).keySet)) true
    else if (win(3).subsetOf(moveLog.filter(_._2 == player).keySet)) true
    else if (win(4).subsetOf(moveLog.filter(_._2 == player).keySet)) true
    else if (win(5).subsetOf(moveLog.filter(_._2 == player).keySet)) true
    else if (win(6).subsetOf(moveLog.filter(_._2 == player).keySet)) true
    else if (win(7).subsetOf(moveLog.filter(_._2 == player).keySet)) true
    else false}

  val tMoves: Set[TMove] = Set(TopLeft, TopCenter, TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)
  val fieldsNotSet = moveLog.filter(x => x._2 != PlayerA || x._2 != PlayerB).keySet
  val remainingMoves: Set[TMove] = tMoves.diff(fieldsNotSet)
  val gameOver: Boolean = true match {case gameEnd => winner.isDefined || remainingMoves.isEmpty;case _ => false}
  def winner: Option[(Player, Set[TMove])] = {if (checkWon(PlayerA)) Some(PlayerA, moveLog.filter(_._2 == PlayerA).keySet) else if (checkWon(PlayerB)) Some(PlayerB, moveLog.filter(_._2 == PlayerB).keySet);else None};
  def turn(p: TMove, player: Player): TicTacToe = {if (!moveLog.get(p).contains(PlayerA) || !moveLog.get(p).contains(PlayerB)) {if (player.equals(PlayerA)) TicTacToe(this.moveLog + (p -> player), PlayerB);else TicTacToe(this.moveLog + (p -> player), PlayerA)};else {TicTacToe(this.moveLog, nextPlayer)}}};