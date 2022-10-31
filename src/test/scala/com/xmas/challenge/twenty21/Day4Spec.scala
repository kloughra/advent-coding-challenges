package com.xmas.challenge.twenty21

import com.xmas.challenge.twenty21.Day4.{Board, Position, leftDiagonalIndices, rightDiagonalIndices,prettyPrintBoards}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 4"

  val playList: List[Int] = List(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)
  val boards: List[Board] = List(
    List(
      List(22,13,17,11,0),
      List(8,2,23,4,24),
      List(21,9,14,16,7),
      List(6,10,3,18,5),
      List(1,12,20,15,19)
    ),
    List(
      List(3,15,0,2,22),
      List(9,18,13,17,5),
      List(19,8,7,25,23),
      List(20,11,10,24,4),
      List(14,21,16,12,6)
    ),
    List(
      List(14,21,17,24 ,4),
      List(10,16,15,9,19),
      List(18,8,23,26,20),
      List(22,11,13,6,5),
      List(2,0,12,3,7)
    )
  ).map( board ⇒ board.map(row ⇒ row.map(Position(_))))


  it should "apply list of drawn numbers to a board" in {
    1 shouldBe 1
  }

  it should "check board for row bingo" in {
    val board: Board = boards.head
      .zipWithIndex.map(x ⇒ {
      if(x(1) == 3) x(0).map(_.copy(placed = true))
      else x(0)
    })
    prettyPrintBoards(List(board))
    Day4.checkBoardForBingo(board) shouldBe true
  }

  it should "check empty board for no bingo" in {
    prettyPrintBoards(List(boards.head))
    Day4.checkBoardForBingo(boards.head) shouldBe false
  }

  it should "check board for column bingo" in {
    val board: Board = boards.head
      .map( row ⇒ row.zipWithIndex.map(p ⇒ if(p(1)==2) p(0).copy(placed = true) else p(0)))
    prettyPrintBoards(List(board))
    Day4.checkBoardForBingo(board) shouldBe true
  }

  it should "check board for left diagonal bingo" in {
    val board: Board = boards.head
    val bingoedSeq = for(bi ← board.indices) yield {
      val seq = for(ri ← 0 until 5) yield {
        if (leftDiagonalIndices.contains((bi, ri))) board(bi)(ri).copy(placed = true)
        else board(bi)(ri)
      }
      seq.toList
    }

    val bingoedBoard = bingoedSeq.toList

    prettyPrintBoards(List(bingoedBoard))
    Day4.checkBoardForBingo(bingoedBoard) shouldBe true
  }
  it should "check board for right diagonal bingo" in {
    var board: Board = boards.head
    val bingoedSeq = for(bi ← board.indices) yield {
      val seq = for(ri ← 0 until 5) yield {
        if (rightDiagonalIndices.contains((bi, ri))) board(bi)(ri).copy(placed = true)
        else board(bi)(ri)
      }
      seq.toList
    }

    val bingoedBoard = bingoedSeq.toList

    prettyPrintBoards(List(bingoedBoard))
    Day4.checkBoardForBingo(bingoedBoard) shouldBe true
  }
}
