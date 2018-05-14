package object NumberModule {
    def gcd(a: Int, b: Int): Int = 
        if (a == 0) b
        else if (b == 0) a
        else {
            val d = a % b
            gcd(b, d)
        }
}
