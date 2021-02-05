import scala.io.StdIn._

/**
  * There is no Spoon: Episode 1
  * https://www.codingame.com/training/medium/there-is-no-spoon-episode-1
  * by skelantros
  */

object Player extends App {
    val width = readInt()
    val height = readInt()
    val grid = Array.fill(height, width)(false)
    for(i <- 0 until height) {
        readLine().zipWithIndex.foreach(t => grid(i)(t._2) = (t._1 == '0'))
    }

    val points = collection.mutable.Set[Point]()
    val rightNeighbors = collection.mutable.Map[Point,Point]()
    for(h <- 0 until height) {
        var prev = Point(-1, -1)
        for(w <- 0 until width if grid(h)(w)) {
            rightNeighbors(prev) = Point(w, h)
            prev = Point(w, h)
            points += Point(w, h)
        }
    }

    val downNeighbors = collection.mutable.Map[Point, Point]()
    for(w <- 0 until width) {
        var prev = Point(-1, -1)
        for(h <- 0 until height if grid(h)(w)) {
            downNeighbors(prev) = Point(w, h)
            prev = Point(w, h)
            points += Point(w, h)
        }
    }

    for(p <- points) {
        val r = rightNeighbors.get(p).getOrElse(Point(-1, -1))
        val d = downNeighbors.get(p).getOrElse(Point(-1, -1))
        println(s"$p $r $d")
    }
}

case class Point(x : Int, y : Int) {
    override def toString = s"$x $y"
}