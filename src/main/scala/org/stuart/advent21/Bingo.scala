package org.stuart.advent21

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class Bingo {

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
    val sortedStats = turnCountByBoard.sortBy(_._2)
    val winFirstScore = sortedStats.head._3 * boards(sortedStats.head._1).score
    val winLastScore = sortedStats.last._3 * boards(sortedStats.last._1).score
    (winFirstScore, winLastScore)
  }
}
