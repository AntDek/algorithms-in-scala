package patternRecognition

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunPoint extends FunSuite {
	test("test slope with NegativeInfinity") {
		val x = new Point(1, 3)
		val y = new Point(1, 3)
		assert( (x slopeTo y) == Double.NegativeInfinity )
	}

	test("test slope with PositiveInfinity") {
		val x = new Point(1, 5)
		val y = new Point(1, 3)
		assert( (x slopeTo y) == Double.PositiveInfinity )
	}

	test("test slope with zero") {
		val x = new Point(3, 3)
		val y = new Point(1, 3)
		assert( (x slopeTo y) == 0.0 )
	}

	test("test slope") {
		val x = new Point(1, 3)
		val y = new Point(4, 5)
		assert( (x slopeTo y) == (y.y - x.y).toDouble / (y.x - x.x).toDouble )
	}
}