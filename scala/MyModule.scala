object MyModule {
    def abs(n: Int): Int = 
        if (n < 0) -n
        else n

    def factorial(n: Int): Int = 
        if (n <= 0) 1
        else n * factorial(n-1)

    def findFirst[A] (as: Array[A], p: A => Boolean): Int = {
        def iterFind(n: Int): Int = 
            if (n >= as.length) -1
            else if (p(as(n))) n
            else iterFind (n+1)

        iterFind(0)    
    }   

    def isSorted[A](xs: Array[A], ordered: (A,A) => Boolean): Boolean = {
        def loop(n: Int): Boolean = 
            if (n >= xs.length) true
            else if (ordered(xs(n-1), xs(n))) loop(n+1)
            else false

        loop(1)
    }

    private def formatAbs(x: Int) = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }

    private def formatFactorial(n: Int) = {
        val msg = "The factorial of %d is %d"
        msg.format(n, factorial(n))
    }

    def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d"
        msg.format(name, n, f(n))
    }

    def main(args: Array[String]): Unit = {
        println(formatAbs(-42))
        println(formatFactorial(7))
    }

}