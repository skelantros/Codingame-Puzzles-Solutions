import scala.io.StdIn._

/**
  * Shadows of the Knight: Episode 1
  * https://www.codingame.com/ide/puzzle/shadows-of-the-knight-episode-1
  * by skelantros
  **/

object Player extends App {
    val Array(w, h) = (readLine() split " ").map (_.toInt)
    val Array(x0, y0) = (readLine() split " ").map (_.toInt)

    val rectView = new RectangleView(Point(0, 0), Point(w - 1, h - 1))
    var nextPoint = Point(x0, y0)
    while(true) {
        rectView.update(Direction.withName(readLine()), nextPoint)
        nextPoint = rectView.nextPoint()
        println(nextPoint.toString)
    }
}

class Point(xVal : Int, yVal : Int) {
    val x = if(xVal < 0) 0 else if(xVal >= Player.w) Player.w - 1 else xVal
    val y = if(yVal < 0) 0 else if(yVal >= Player.h) Player.h - 1 else yVal
    override def toString = s"$x $y"
}
object Point {
    def apply(x : Int, y : Int) = new Point(x, y)
}

object Direction extends Enumeration {
    val U, UR, R, DR, D, DL, L, UL = Value
}
class RectangleView(var p1 : Point, var p2 : Point) {
    def nextPoint(): Point = Point((p1.x + p2.x) / 2, (p1.y + p2.y) / 2)
    import Direction._
    def update(dir : Direction.Value, p : Point): Unit = dir match {
        case U  => { p1 = Point(p.x, p1.y); p2 = Point(p.x, p.y - 1) }
        case D  => { p2 = Point(p.x, p2.y); p1 = Point(p.x, p.y + 1) }
        case R  => { p1 = Point(p.x + 1, p.y); p2 = Point(p2.x, p.y) }
        case L  => { p2 = Point(p.x - 1, p.y); p1 = Point(p1.x, p.y) }
        case UR => { p1 = Point(p.x + 1, p1.y); p2 = Point(p2.x, p.y - 1) }
        case DL => { p1 = Point(p1.x, p.y + 1); p2 = Point(p.x - 1, p2.y) }
        case UL => { p2 = Point(p.x, p.y - 1) }
        case DR => { p1 = Point(p.x + 1, p.y) }
    }
}