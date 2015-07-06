package patternRecognition

class Point (val x: Int, val y: Int) extends Ordered[Point] {

	def compare(that: Point): Int = {
		if (y == that.y && x == that.x) 0
		else if (y == that.y && x > that.x) 1
		else if (y > that.y) 1
		else -1
	}

	def slopeTo(that: Point): Double = {
		if (that.y == y && that.x == x) Double.NegativeInfinity
		else if (that.x == x) Double.PositiveInfinity
		else if (that.y == y) 0.0
		else (that.y - y).toDouble / (that.x - x).toDouble
	}

	override def toString: String = "(" + x + "," + y + ")"
}