package patternRecognition

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunRecognition extends FunSuite {

	trait RecognitionTest {
		def fetchPoints(grid: String): List[Point] = {
			val matrix = toMatrix(grid)
			toAxis(matrix.reverse) map { case (i, j) =>
				new Point(i, j)
			}
		}

		private def toMatrix(grid: String): List[List[Char]] = {
			grid.split("\n").map(_.toList).toList
		}

		private def toAxis(matrix: List[List[Char]]): List[Tuple2[Int, Int]] = {
			(for {
				i <- 0 until matrix.length
				j <- 0 until matrix(i).length
				if matrix(i)(j) == '0'
			} yield (i+1, j+1)) toList
		}
	}

	test("Brute recognition should recognise line") {
		new RecognitionTest {
			val points = fetchPoints(
				"""-0-0-
				  |--0--
				  |-0-0-
				  |0----""".stripMargin
			)

			val lines = Recognition.brute(points)
			assert(lines.head.mkString("-") == "(1,1)-(2,2)-(3,3)-(4,4)")
		}
	}

	test("Fast recognition should recognise a line") {
		new RecognitionTest {
			val points = fetchPoints(
				"""-0-0-
				  |--0--
				  |-0-0-
				  |0----""".stripMargin
			)

			val lines = Recognition.fast(points)
			assert(lines.head.mkString("-") == "(1,1)-(2,2)-(3,3)-(4,4)")
		}
	}

	test("Fast recognition should find all lines") {
		new RecognitionTest {
			val points = fetchPoints(
				"""---00-
				  |--00--
				  |-000--
				  |000-0-""".stripMargin
			)

			val lines = Recognition.fast(points)
			assert(lines.length == 3)
		}
	}

	test("Fast recognition should find all lines without duplicates") {
		new RecognitionTest {
			val points = fetchPoints(
				"""--0-0-
				  |--00--
				  |--0---
				  |-00---
				  |0-0---""".stripMargin
			)

			val lines = Recognition.fast(points)
			assert(lines.length == 2)
		}
	}
}