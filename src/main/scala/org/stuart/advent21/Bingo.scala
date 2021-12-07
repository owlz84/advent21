package org.stuart.advent21

import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class Bingo extends LazyLogging {

  class Row(var squares: ArrayBuffer[Int]) extends ArrayBuffer {
    def pretty = squares.mkString("\t")
  }

  class Board(var rows: ArrayBuffer[Row]) extends ArrayBuffer {
    val lenSides = rows.length
    var rowCount = ArrayBuffer.fill[Int](lenSides)(0)
    var colCount = ArrayBuffer.fill[Int](lenSides)(0)
    def complete = (rowCount ++ colCount).filter(_ == lenSides).length > 0
    def score = rows.map(row => row.squares.filter(_ >= 0).sum).sum
    def pretty = rows.map(_.pretty).mkString("\n")
  }

  var boards: ArrayBuffer[Board] = _
  var turnCountByBoard = new ListBuffer[(Int, Int, Int)]

  def configure(boardData: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]) = {
    boards = boardData.map(board => {
      new Board(board.map(row => {
        new Row(row)
      }))
    })
  }

  def calcTurnsUntilWin(nums: List[Int]) {
    turnCountByBoard.clear()
    boards.zipWithIndex.foreach({
      case (board: Board, i: Int) => {
        nums.zipWithIndex.toStream
          .takeWhile(_ => !board.complete)
          .foreach({
            case (num: Int, turn: Int) => {
              for {
                (row, j) <- board.rows.zipWithIndex
                (square, k) <- row.squares.zipWithIndex
                if (square == num)
              } {
                board.rows(j).squares(k) = -1
                board.colCount(j) += 1
                board.rowCount(k) += 1
                if (
                  board.colCount(j) >= board.lenSides | 
                  board.rowCount(k) >= board.lenSides
                ) {
                  turnCountByBoard += ((i, turn, num))
                }
              }
            }
          })
      }
    })
  }

  def play(nums: List[Int]) = {
    calcTurnsUntilWin(nums)
    val winFirstStats = turnCountByBoard.sortBy(_._2).head
    val winFirstScore = winFirstStats._3 * boards(winFirstStats._1).score
    val winLastStats = turnCountByBoard.sortBy(_._2).last
    val winLastScore = winLastStats._3 * boards(winLastStats._1).score
    // logger.debug(winLastStats.toString)
    // logger.debug(winLastScore.toString)
    // logger.debug(boards(winLastStats._1).pretty)
    (winFirstScore, winLastScore)
  }
}
