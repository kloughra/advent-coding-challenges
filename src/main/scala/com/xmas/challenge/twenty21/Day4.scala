package com.xmas.challenge.twenty21

import scala.annotation.tailrec
import scala.io.Source

/**
 * Day 4 - Advent of Code 2021
 * Bingo
 * https://adventofcode.com/2021/day/4
 */
object Day4 {

  private lazy val day4File: String = "Day4_2021_Input.txt"
  lazy val defaultInput: List[String] = Source.fromResource(day4File)
    .getLines.toList

  lazy val defaultRules = defaultInput.head.split(",").toList.map(_.toInt)

  lazy val defaultBoards: List[Board] =
    defaultInput.tail
      .filter(_ != "")
      .map(_.split(" ").toList.filter(_ != "").map(p ⇒ Position(p.toInt)))
      .sliding(5,5).toList


  case class Position(value: Int, placed: Boolean = false)
  type Board = List[List[Position]]

  def playBingo(rules: List[Int], boards: List[Board]): Int = {
    val winningBoard = applyRulesToBoards(rules, boards)
    winningBoard.map( b ⇒ calculateWinningScore(b(0),b(2))).getOrElse(-1)
  }

  def playBingoToLose(rules: List[Int], boards: List[Board]): Int = {
    val (rule, boardCount, board) = applyRulesToBoardsToLose(rules, boards)
    println(s"Rule: $rule, board $boardCount")
    calculateWinningScore(rule, board)
  }
  def calculateWinningScore(rule: Int, board: Board): Int = {
    val boardSum: Int = board.flatMap(row ⇒ row.filter(pos ⇒ !pos.placed))
      .foldLeft(0)((sum,pos) ⇒ sum + pos.value)
    boardSum * rule
  }

  // not tail rec
  def applyRulesToBoards(rules: List[Int], boards: List[Board]): Option[(Int,Int,Board)] = rules match {
    case Nil ⇒
      println("No Bingo Found")
      None
    case rule::tail ⇒
      println(s"Applying Rule: $rule")
      val updatedBoards = boards.map(board ⇒ applyRuleToBoard(rule,board))
      val bingo: Option[(Boolean,Int)] = updatedBoards.map(checkBoardForBingo).zipWithIndex.find(b ⇒ b(0) == true)
      if(bingo.isEmpty){
        applyRulesToBoards(tail, updatedBoards)
      } else bingo.map(b ⇒ (rule, b(1) + 1, updatedBoards(b(1)))) // add one to index to get board number
  }

  @tailrec
  def applyRulesToBoardsToLose(rules: List[Int], boards: List[Board], winningBoards: List[(Int,Int,Board)] = List()): (Int,Int,Board) = rules match {
    case Nil ⇒
      println("All Rules Played. Winning Boards: ")
      winningBoards.head
    case rule::tail ⇒
      println(s"Applying Rule To Lose: $rule")
      //Exclude Boards Already Won
      val boardsAlreadyWon: List[Int] = winningBoards.map(b ⇒ b(1))

      val updatedBoards: List[Board] = boards.zipWithIndex.map(board ⇒ {
        if(boardsAlreadyWon.contains(board(1))) board(0)
        else applyRuleToBoard(rule,board(0))
      })
      val bingo: List[(Boolean,Int)] =
        updatedBoards.zipWithIndex
          .filter(b ⇒ !boardsAlreadyWon.contains(b(1)))
          .map(b ⇒ (checkBoardForBingo(b(0)), b(1)))
          .filter(b ⇒ b(0) == true)

      applyRulesToBoardsToLose(tail, updatedBoards, bingo.map(b ⇒ (rule, b(1), updatedBoards(b(1)))) ::: winningBoards)
  }
  private def applyRuleToBoard(rule: Int, board:Board): Board = {
    board.map(row ⇒ row.map(place ⇒ if(place.value == rule) place.copy(placed = true) else place))
  }

  //checking whole board, could optimize to only check rows and columns for new placement
  //could also do a recursive function to return true as soon as a find a bingo
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
    //val isLeftDiagonalBingo: Boolean = leftDiagonalIndices.foldLeft(true)((bingo,ind) ⇒ board(ind(0))(ind(1)).placed && bingo)
    //val isRightDiagonalBingo: Boolean = rightDiagonalIndices.foldLeft(true)((bingo,ind) ⇒ board(ind(0))(ind(1)).placed && bingo)

    isRowBingo || isColumnBingo  //|| isLeftDiagonalBingo || isRightDiagonalBingo
  }


  /**
   * Helpers for Debugging & Test
   */

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
