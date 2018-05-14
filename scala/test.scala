def compose[A,B,C](f: B=>C, g: A=>B): A=>C = (x: A) => f(g(x)) 

def twice[A](f: A=>A) = compose(f,f)

def inc(x: Int) = x + 1

object Main {
    def main(args: Array[String]): Unit = {
        println("Twice: %d", twice(inc)(10))
    }
}

sealed trait Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

case class Point (
    x: Int,
    y: Int
)

case class Person (
    firstName: String,
    lastName: String,
    mother: Person,
    father: Person
)

def move(d: Direction, p: Point): Point = d match {
    case North => Point(p.x, p.y + 1)
    case South => Point(p.x, p.y - 1)
    case East => Point(p.x + 1, p.y)
    case West => Point(p.x - 1, p.y)
}

// def moves(ds: List[Direction], p: Point): Point
