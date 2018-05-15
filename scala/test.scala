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

sealed trait Shape
final case class Circle(radius: Double) extends Shape
final case class Rectangle(width: Double, height: Double) extends Shape

case class Node (
    data: Int,
    next: Node
)

sealed trait Option[+A]
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A] 

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

def sum(x: Int, y: Int) = x + y

def currySum = curry(sum)

def sum1 = currySum(1)

