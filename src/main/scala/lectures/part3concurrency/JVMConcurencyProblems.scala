package lectures.part3concurrency

object JVMConcurencyProblems {
  def runInParallel(): Unit = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })
    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x) // race condition [can be 1 or 2]
  }

  case class BankAccount(var amount: Int)

  def buy(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    bankAccount.amount -= price
  }

  def buySafe(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    bankAccount.synchronized({ // does not allow multiple multiple thread to run the critical section AT THE SAME TIME
      bankAccount.amount -= price // critical section
    })
  }

  /*
    Example race condition:
    thread1 (shoes)
      - reads amount 50000
      - compute result 50000 - 3000 = 47000
    thread2 (iPHone)
      - reads amount 50000
      - compute result 50000 - 3000 = 46000
    thread1 (shoes)
      - write amount 47000
    thread2 (iPhone)
      - write amount 46000 [overrides thread1]
  */
  def demoBankingProblem(): Unit = {
    (1 to 100000).foreach { _ =>
      val account = BankAccount(50000)
      val thread1 = new Thread(() => buy(account, "shoes", 3000))
      val thread2 = new Thread(() => buy(account, "iPhone", 4000))
      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()
      if (account.amount != 43000) println(s" broken bank: ${account.amount}")
    }
  }

  /**
   * Exercises
   *
   * 1) Construct 50 "inception" threads
   * Thread1 -> thread2 -> thread3 -> ...
   * println("hello from thread #3")
   * in REVERSE ORDER
   *
   */
  // 1 - inception threads
  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread =
    new Thread(() => {
      if (i < maxThreads) {
        val newThread = inceptionThreads(maxThreads, i + 1)
        newThread.start()
        newThread.join()
      }
      println(s"Hello from thread $i")
    })


  //  2

  def minMax(): Unit = {
    var x = 0
    val threads = (1 to 100).map(_ => new Thread(() => x += 1))
    threads.foreach(_.start())
  }
  /*
    max value = 100 - each thread increases x by 1
    min value = 1
      all threads read x = 0 at the same time
      all threads (in parallel) compute 0 + 1 = 1
      all threads try to write x = 1
   */


  // 3 sleep fallacy
  /*
    almost always, message = "Scala os awesome"
    is it guranteed? NO
    Obnoxious situation (possible):

    main thread:
      meassage = "Scala sucks"
      awesomeThread.start()
      sleep(1001) - yields execution
    awesome thread:
      sleep(1000) - yields execution
    OS gives the CPU to some important thread, takes > 2s
    OS gives the CPU back to the main thread
    main thread:
      println(message) // "Scala sucks"
    awesome thread:
      println = "Scala is awesome"

  */
  def demoSleepFallacy(): Unit = {
    var message = ""
    val awesomeThread = new Thread(() => {
      Thread.sleep(1000)
      message = "Scala is awesome"
    })

    message = "Scala sucks"
    awesomeThread.start()
    Thread.sleep(1001)
    // solution : join the worker thread
    // synchronizationdoes not work
    awesomeThread.join()
    println(message)
  }

  def main(args: Array[String]): Unit = {
    demoSleepFallacy()
  }

}
