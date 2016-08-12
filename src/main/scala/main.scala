package main

import scala.annotation.tailrec
import scala.io.StdIn.{readLine}

object Main {
  case class Row[A](left: A, middle: A, right: A)
  case class Col[A](top: Row[A], middle: Row[A], bottom: Row[A])

  case class Board(matrix: Col[Option[PlayerChoice]])

  sealed trait PlayerChoice
  case object Xs extends PlayerChoice
  case object Os extends PlayerChoice

  case class GameState(board: Board, player: PlayerChoice)

  sealed trait UserError
  case object InvalidMove extends UserError
  case object GameOver extends UserError

  case class CurrentInput(col: Int, row: Int)

  def alternateTurn(state: GameState): GameState =
    state.player match {
      case Xs => state.copy(player=Os)
      case Os => state.copy(player=Xs)
    }

  def update(userInput: CurrentInput, state: GameState): Either[UserError, GameState]  = {
    userInput match {
      case CurrentInput(1,1) =>
        if (state.board.matrix.top.left != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(left=Some(state.player)), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy())))))
      case CurrentInput(1,2) =>
        if (state.board.matrix.top.middle != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(middle=Some(state.player)), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy())))))
      case CurrentInput(1,3) =>
        if (state.board.matrix.top.middle != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(right=Some(state.player)), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy())))))
      case CurrentInput(2,1) =>
        if (state.board.matrix.top.left != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(left=Some(state.player)), bottom = state.board.matrix.bottom.copy())))))
      case CurrentInput(2,2) =>
        if (state.board.matrix.top.middle != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(middle=Some(state.player)), bottom = state.board.matrix.bottom.copy())))))
      case CurrentInput(2,3) =>
        if (state.board.matrix.top.middle != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(right=Some(state.player)), bottom = state.board.matrix.bottom.copy())))))
      case CurrentInput(3,1) =>
        if (state.board.matrix.top.left != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy(left=Some(state.player)))))))
      case CurrentInput(3,2) =>
        if (state.board.matrix.top.middle != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy(middle=Some(state.player)))))))
      case CurrentInput(3,3) =>
        if (state.board.matrix.top.middle != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy(right=Some(state.player)))))))
    }
  }

  def getInput: CurrentInput =
    {
      val col = readLine("Col 1, 2, or 3\n").toInt
      val row = readLine("Row 1, 2, or 3\n").toInt
      CurrentInput(col, row)
    }

  def showError(err: UserError): Unit = ???

  def printBoard(state: GameState): Unit =
    println(state.board)

  @tailrec
  def loop(state: GameState): Unit = {
    val userInput = getInput

    update(userInput, state) match {
      case Left(error) => showError(error); loop(state)
      case Right(state) => printBoard(state); loop(state)
    }
  }

  def main(args: Array[String]): Unit = {
    val initialState =
      GameState(Board(Col[Option[PlayerChoice]](
        Row(None, None, None),
        Row(None, None, None),
        Row(None, None, None))), Xs)
    printBoard(initialState)
    loop(initialState)
  }
}