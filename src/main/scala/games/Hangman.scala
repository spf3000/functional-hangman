package games

import scalaz._
import Scalaz._
import zio._
import random._
import console._

import scala.language.higherKinds
import java.io.IOException

/**
  * Hangman game
  *   1. choose a random word
  *   2. show user word with unguessesed letters hidden
  *   3. ask the user to guess a letter
  *     - if the letter has been guessed before
  *       - let the user know and go back to 3.
  *   - if the letter is not in the word
  *       - inform user and increment error count
  *         - if error count is less than 6 go to 3.
  *         - else end game if it reaches 6
  *     - if the letter is in the word
  *       - add the letter to the guess, inform the user and go to 3.
  *       - if the word as all letters guessed end game
  */
object Hangman extends App {

  case class State(name: String, guesses: Set[Char], word: String) {
    final val failures: Int       = (guesses -- word.toSet).length
    final val playerLost: Boolean = failures > 7
    final val playerWon: Boolean  = (word.toSet -- guesses).isEmpty
  }

  val chooseWord: ZIO[Random, Nothing, String] =
    nextInt(Dictionary.length)
      .map(int => (Dictionary).lift(int).getOrElse("bug in the program").toLowerCase)


  val getChoice: ZIO[Console, IOException, Char] =
    for {
      _    <- putStrLn("Please guess a letter")
      line <- getStrLn
      char <- line.toLowerCase.trim.headOption match {
        case None =>
          putStrLn("You did not enter a letter") *> getChoice
        case Some(c) => ZIO.succeed(c)
      }
    } yield char

  def renderState(state: State): ZIO[Console, IOException, Unit] = {
    //
    // f   n c t o
    // _ _ _ _ _ _ _
    //
    // Guesses: a, z, y, x
    //
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) c else " ")
        .mkString(" ")

    val placeholders =
      state.word.toList.map(_ => "_").mkString(" ")

    val guesses =
      state.guesses.toList.mkString(", ")

    for {
      _ <- putStrLn("")
      _ <- putStrLn(word)
      _ <- putStrLn(placeholders)
      _ <- putStrLn("")
      _ <- putStrLn(s"Guesses: $guesses")
      _ <- putStrLn("")
    } yield ()
  }

  def gameLoop(state: State): ZIO[Console, IOException, State] =
    for {
      guess <- getChoice
      state <- ZIO.succeed(state.copy(guesses = state.guesses + guess))
      _     <- renderState(state)
      loop <- if (state.playerWon)
        putStrLn(s"Congratulations, ${state.name}, you won the game!!!").const(false)
        else if (state.playerLost)
          putStrLn(s"Sorry, ${state.name}, you lost the game. The word was ${state.word}")
            .const(false)
        else if (state.word.contains(guess))
          putStrLn(s"You guessed correctly, ${state.name}, keep going!").const(true)
        else
          putStrLn(s"You guessed wrong, ${state.name}, try again.").const(true)
      state <- if (loop) gameLoop(state) else ZIO.succeed(state)
    } yield state

  val hangmanGame: ZIO[Console with Random, IOException, Unit] =
    for {
      _    <- putStrLn("Functional Hangman")
      name <- putStrLn("What is your name?") *> getStrLn
      _    <- putStrLn(s"Welcome, $name")
      word <- chooseWord
      state = State(name, Set(), word)
      _ <- renderState(state)
      _ <- gameLoop(state)
    } yield ()

  override def run(args: List[String]) = hangmanGame.fold(_ => 1, _ => 0)

  lazy val Dictionary = List(
    "great",
    "british",
    "bake",
    "off",
    "salt",
    "fat",
    "acid",
    "heat",
    "one",
    "punch",
    "man",
    "wrinkle",
    "face",
    "hat",
    "cinnamon",
    "scaffold",
    "wonderful",
    "egregious",
    "plentiful",
    "bacon",
    "knife",
    "eucalyptus",
    "treasure",
    "femur",
    "acres",
    "adult",
    "advice",
    "arrangement",
    "attempt",
    "August",
    "Autumn",
    "border",
    "breeze",
    "brick",
    "calm",
    "canal",
    "cast",
    "chose",
    "claws",
    "coach",
    "constantly",
    "contrast",
    "cookies",
    "customs",
    "damage",
    "Danny",
    "deeply",
    "depth",
    "discussion",
    "doll",
    "donkey",
    "Egypt",
    "essential",
    "exchange",
    "exist",
    "explanation",
    "facing",
    "film",
    "finest",
    "fireplace",
    "floating",
    "folks",
    "fort",
    "garage",
    "grabbed",
    "grandmother",
    "habit",
    "happily",
    "heading",
    "hunter",
    "Illinois",
    "image",
    "independent",
    "instant",
    "January",
    "kids",
    "label",
    "Lee",
    "lungs",
    "manufacturing",
    "mathematics",
    "melted",
    "memory",
    "mill",
    "mission",
    "monkey",
    "Mount",
    "mysterious",
    "neighborhood",
    "Norway",
    "nuts",
    "occasionally",
    "official",
    "ourselves",
    "palace",
    "Pennsylvania",
    "Philadelphia",
    "plates",
    "poetry",
    "policeman",
    "positive",
    "possibly",
    "practical",
    "pride",
    "promised",
    "recall",
    "relationship",
    "remarkable"
  )
}
