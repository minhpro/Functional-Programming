object MyModule {
    def abs(n: Int): Int = 
        if (n < 0) -n
        else n

    def factorial(n: Int): Int = 
        if (n <= 0) 1
        else n * factorial(n-1)

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