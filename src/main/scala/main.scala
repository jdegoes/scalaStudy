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

  def sameKind(a: PlayerChoice, b: PlayerChoice, c: PlayerChoice): Boolean =
    // You can also write this as (a == b && b == c)
    if (a == Xs && b == Xs && c == Xs || a == Os && b == Os && c == Os) true else false

  def checkForEnd(userInput: CurrentInput, state: GameState): Either[UserError, (CurrentInput, GameState)]  = {
    // This function has a lot of boilerplate — I'd recommend seeing if you can find a way to reduce it
    state.board match {
      case Board(Col(
        Row(Some(a), Some(b), Some(c)),
        Row(_, _, _),
        Row(_, _, _))) =>
          if (sameKind(a,b,c)) Left(GameOver)
          else
            Right(userInput, state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy()))))
      case Board(Col(
        Row(_, _, _),
        Row(Some(a), Some(b), Some(c)),
        Row(_, _, _))) =>
          if (sameKind(a,b,c)) Left(GameOver)
          else
            Right(userInput, state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy()))))
      case Board(Col(
        Row(_, _, _),
        Row(_, _, _),
        Row(Some(a), Some(b), Some(c)))) =>
          if (sameKind(a,b,c)) Left(GameOver)
          else
            Right(userInput, state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy()))))
      case Board(Col(
        Row(Some(a), _, _),
        Row(Some(b), _, _),
        Row(Some(c), _, _))) =>
          if (sameKind(a,b,c)) Left(GameOver)
          else
            Right(userInput, state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy()))))
      case Board(Col(
        Row(_, Some(a), _),
        Row(_, Some(b), _),
        Row(_, Some(c), _))) =>
          if (sameKind(a,b,c)) Left(GameOver)
          else
            Right(userInput, state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy()))))
      case Board(Col(
        Row(_, _, Some(a)),
        Row(_, _, Some(b)),
        Row(_, _, Some(c)))) =>
          if (sameKind(a,b,c)) Left(GameOver)
          else
            Right(userInput, state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy()))))
      case Board(Col(
        Row(Some(a), _, _),
        Row(_, Some(b), _),
        Row(_, _, Some(c)))) =>
          if (sameKind(a,b,c)) Left(GameOver)
          else
            Right(userInput, state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy()))))
      case Board(Col(
        Row(_, _, Some(a)),
        Row(_, Some(b), _),
        Row(Some(c), _, _))) =>
          if (sameKind(a,b,c)) Left(GameOver)
          else
            Right(userInput, state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy()))))
      case _ => Right(userInput, state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy()))))
    }
  }

  def update(userInput: CurrentInput, state: GameState): Either[UserError, GameState]  = {
    // Same here — see if there's a way you can simplify some of this (it's not easy, though, unless you change the board matrix representation)
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
        if (state.board.matrix.top.right != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(right=Some(state.player)), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy())))))
      case CurrentInput(2,1) =>
        if (state.board.matrix.middle.left != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(left=Some(state.player)), bottom = state.board.matrix.bottom.copy())))))
      case CurrentInput(2,2) =>
        if (state.board.matrix.middle.middle != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(middle=Some(state.player)), bottom = state.board.matrix.bottom.copy())))))
      case CurrentInput(2,3) =>
        if (state.board.matrix.middle.right != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(right=Some(state.player)), bottom = state.board.matrix.bottom.copy())))))
      case CurrentInput(3,1) =>
        if (state.board.matrix.bottom.left != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy(left=Some(state.player)))))))
      case CurrentInput(3,2) =>
        if (state.board.matrix.bottom.middle != None)
          Left(InvalidMove)
        else
          Right(alternateTurn(state.copy(state.board.copy(state.board.matrix.copy(top = state.board.matrix.top.copy(), middle = state.board.matrix.middle.copy(), bottom = state.board.matrix.bottom.copy(middle=Some(state.player)))))))
      case CurrentInput(3,3) =>
        if (state.board.matrix.bottom.right != None)
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

  def showError(err: UserError): Unit =
    println(err)

  def printBoard(state: GameState): Unit =
    println(state.board)

  @tailrec
  def loop(state: GameState): Unit = {
    val userInput = getInput

    checkForEnd(userInput, state) match {
      case Left(error) => showError(error)
      case Right((userInput, state)) =>
        update(userInput, state) match {
          case Left(error) => showError(error); loop(state)
          case Right(state) => printBoard(state); loop(state)
        }
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
