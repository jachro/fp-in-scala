package ch2dot4

object Fibonacci {

  // First number is 1
  def findNumber(n: Int): Int = {
    require(n > 0, "First number is 1")

    @annotation.tailrec
    def fib(counter: Int, previous: Int, acc: Int): Int = {
      if (counter > 0)
        fib(counter - 1, acc, acc + previous)
      else
        acc
    }

    if (n == 1)
      0
    else if (n == 2)
      1
    else
      fib(n - 3, 1, 1)
  }

  // First number is 1
  def findNumberCompletelyInLocalFunction(n: Int): Int = {
    require(n > 0, "First number is 1")

    @annotation.tailrec
    def fib(n: Int, counter: Int, previous: Int, acc: Int): Int = {
      if (n == 1)
        0
      else if (counter == 1)
        fib(n, counter + 1, 0, 1)
      else if (counter < n)
        fib(n, counter + 1, acc, acc + previous)
      else
        acc
    }

    fib(n, 1, 0, 0)
  }

}
