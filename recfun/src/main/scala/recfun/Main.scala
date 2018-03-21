package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0) 1
      else if (c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(chars: List[Char], nl: Int, nr: Int): Boolean = {
        if (chars.isEmpty) nl == nr
        else if (nl < nr) false
        else if (chars.head == '(') loop(chars.tail, nl + 1, nr)
        else if (chars.head == ')') loop(chars.tail, nl, nr + 1)
        else loop(chars.tail, nl, nr)
      }
      loop(chars, 0, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def loop(money: Int, coins: List[Int]): Int = {
        if (money < 0 ) 0
        else if (money == 0) 1
        else {
          coins.map( coin =>
            loop(money - coin, coins.filter( x => x >= coin))
          ).sum
        }
      }
      loop(money, coins)
    }
  }
