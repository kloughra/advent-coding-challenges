package com.xmas.challenge.twenty21

import scala.io.Source

/**
 * Day4 - Bingo
 */
object Day4 {

  private lazy val day4File: String = "Day4_2021_Input.txt"
  lazy val defaultInput: List[String] = Source.fromResource(day4File)
    .getLines.toList

  case class Position(value: Int, placed: Boolean = false)
  type Board = List[List[Position]]


  // apply rule to board
  // check for bingo

  //  def applyRuleToBoards(rule: Int, boards: List[Board]): Board

  //for now check whole board, could optimize to only check rows and columns for new placement
  //could also do a recursive function to return true as soon as you find a bingo
  def checkBoardForBingo(board: Board): Boolean = {
    //rows board
    val isRowBingo: Boolean = board.exists(row ⇒ {
      row.foldLeft(true)((bingo, pos) ⇒ pos.placed && bingo)
    })

    //columns
    val isColumnBingo: Boolean = (0 to 4).exists(col ⇒ {
      board.foldLeft(true)((bingo, row) ⇒ {
        row(col).placed && bingo
      })
    })

    //diagonals lef
    val isLeftDiagonalBingo: Boolean = leftDiagonalIndices.foldLeft(true)((bingo,ind) ⇒ board(ind(0))(ind(1)).placed && bingo)
    val isRightDiagonalBingo: Boolean = rightDiagonalIndices.foldLeft(true)((bingo,ind) ⇒ board(ind(0))(ind(1)).placed && bingo)

    isRowBingo || isColumnBingo || isLeftDiagonalBingo || isRightDiagonalBingo
  }

  val leftDiagonalIndices: List[(Int,Int)] = List( (0,0), (1,1), (2,2), (3,3), (4,4) )
  val rightDiagonalIndices: List[(Int,Int)] = List( (0,4), (1,3), (2,2), (3,1), (4,0) )

  def prettyPrintBoards(boards: List[Board]): Unit =
    boards.foreach(board ⇒ {
      println("|=============================|")
      println()
      board.foreach(row ⇒ {
        row.foreach(place ⇒ print(padPosition(place)))
        println()
      })
      println()
      println("|=============================|")
    })

  private def padPosition(pos: Position): String = pos.value match
    case i if i <= 9 ⇒ if (pos.placed) s" *$i " else s"  $i "
    case i if 9 < i && i <= 99 ⇒ if (pos.placed) s"*$i " else s" $i "
    case i ⇒ s"$i "
}
