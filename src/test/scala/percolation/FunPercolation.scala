import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunPercolation extends FunSuite {

	trait TestPercolation {
		def createPercolation(grid: String): Percolation = {
			val matrix = toMatrix(grid)
			val percolation = Percolation(matrix.length, matrix(0).length)
			toAxis(matrix) foreach { case (i, j) =>
				percolation open (i, j)
			}
			percolation
		}

		private def toMatrix(grid: String): List[List[Char]] = {
			grid.split("\n").map(_.toList).toList
		}

		private def toAxis(matrix: List[List[Char]]): IndexedSeq[Tuple2[Int, Int]] = {
			for {
				i <- 0 until matrix.length
				j <- 0 until matrix(i).length
				if matrix(i)(j) == '0'
			} yield (i, j)
		}
	}

	test("inserted site should be open") {
		val p = Percolation(5,5)
		p.open(2, 1)
		assert(p.isOpen(2,1))
	}

	test("grid must be perculated") {
		new TestPercolation {
			val percolation = createPercolation(
				"""---00
				  |---0-
				  |--00-""".stripMargin
			)

			assert(percolation.percolates() == true)
		}
	}

	test("grid must not be perculated") {
		new TestPercolation {
			val percolation = createPercolation(
				"""---00
				  |0000-
				  |--000
				  |-----""".stripMargin
			)

			assert(percolation.percolates() == false)
		}
	}

	test("Full site shout be full") {
		new TestPercolation {
			val percolation = createPercolation(
				"""---00
				  |---0-
				  |--00-""".stripMargin
			)

			assert(percolation.isFull(1, 3) == true)
		}
	}

}