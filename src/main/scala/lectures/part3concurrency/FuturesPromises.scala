package lectures.part3concurrency

import scala.concurrent.Future
import scala.util.{Failure, Success}
// important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesPromises extends App {

  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future { // will hold a result of some calculation
    calculateMeaningOfLife // calculates the meaning of life on ANOTHER thread
  } // (global) which is passed by the compiler

  println(aFuture.value) // Option[Try[Int]]

  println("Waiting on the future")
  aFuture.onComplete{
    case Success(meaningOfLife) => println(s"the meaning of life is $meaningOfLife")
    case  Failure(e) => println(s"I have failed with $e")
  } // SOME thread

  Thread.sleep(3000)
}
