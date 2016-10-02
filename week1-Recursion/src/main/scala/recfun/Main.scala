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
      if(r == 0) 1
      else {
        if(c == 0) 1
        else {
          if(r == c) 1
          else {
            if(c == 1) r
            else {
              if(c == r-1) r
              else pascal(c-1, r-1) + pascal(c, r-1)
             }
            }
           }
          }
         } 
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      if(inner(chars, 0) == 0) true
      else false
      }                                             //> balance: (chars: List[Char])Boolean
    
      def inner(a: List[Char], b: Int): Int = {
        if(a.isEmpty) b
        else {
          if(b < 0) b
          else {
            if(a.head == '(') inner(a.tail, b+1)
            else {
              if(a.head == ')') inner(a.tail, b-1)
              else inner(a.tail, b)
          }
         }
        }
       }                           
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
     
      def count(m: Int, c: List[Int]) : Int = {
        if (c.isEmpty || m == 0) 0
        else {
          if (m - c.head == 0) 1
          else {
            if (m - c.head < 0) 0
            else countChange(m - c.head, c) + countChange(m, c.tail)
          }
        }
      }
   
      count(money, coins.sorted)
   }
  }
