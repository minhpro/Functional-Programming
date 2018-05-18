object MyApp {
    
    def doudle(x: Int): Int = 
        2 * x

    def twice(f: Int => Int) = f compose f
}