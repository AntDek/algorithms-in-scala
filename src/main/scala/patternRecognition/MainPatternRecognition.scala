import patternRecognition._

import scala.collection.immutable.TreeSet

object MainPatternRecognition extends App  {

	val points = List(
		new Point(19000, 10000),
		new Point(18000, 10000),
		new Point(32000, 10000),
		new Point(21000, 10000),
		new Point(1234, 5678),
		new Point(14000, 10000),
		new Point(5000, 4000),
		new Point(10000, 0),
		new Point(0, 10000),
		new Point(3000, 7000),
		new Point(7000, 3000),
		new Point(20000, 21000),
		new Point(3000, 4000),
		new Point(14000, 15000),
		new Point(6000, 7000)
	)

	class Pallet {
		StdDraw.setXscale(0, 32768)
		StdDraw.setYscale(0, 32768)

		def drawPoint(point: Point): Unit = {
			StdDraw.point(point.x, point.y)
		}

		def drawLine(from:Point, to:Point): Unit = {
			StdDraw.line(from.x, from.y, to.x, to.y)
		}
	}

	val pallet = new Pallet()

	points foreach pallet.drawPoint

	Recognition.fast(points).foreach { list: List[Point] =>
		list reduceLeft { (from, to) =>
			pallet drawLine (from, to)
			to
		}
	}
}