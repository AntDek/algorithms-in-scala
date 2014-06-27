/**
	Percolation

	A model for many physical systems:
	- N-by-N grid of sites
	- Each site is open with probability p (or blocked with probability 1 - p)
	- System percolates if top and bottom are connected by open sites
**/

trait Percolation {

	val rows: Int

	val columns: Int

	private lazy val topIndex: Int = rows*columns

	private lazy val bottomIndex: Int = rows*columns+1

	private lazy val sites: Array[Boolean] = {
		val grid = new Array[Boolean](rows*columns+2)
		grid(topIndex) = true
		grid(bottomIndex) = true
		grid
	}

	private lazy val unionUF: WeightedQuickUnionUF = new WeightedQuickUnionUF(rows*columns+2)

	def open(i: Int, j: Int) = {
		val index = toIndex(i, j)
		sites(index) = true
		findOpened(i, j) foreach (x =>
			unionUF union (index, x)
		)
	}

	def isOpen(i: Int, j: Int): Boolean = sites(toIndex(i, j))

	def isFull(i: Int, j: Int): Boolean = {
		if (i == 0) return isOpen(i, j)
		(0 until columns).toList exists (column =>
			unionUF connected (column, toIndex(i, j))
		)
	}

	def percolates(): Boolean = unionUF connected (topIndex, bottomIndex)

	private def findOpened(i: Int, j: Int): List[Int] = {
		def extremeIndex: List[Int] = {
			if (i == 0)
				List(topIndex)
			else if (i == rows-1)
				List(bottomIndex)
			else
				Nil
		}

		List((i-1, j), (i+1, j), (i, j-1), (i, j+1))
			.filter { case (i, j) =>
				isInBounds(i, j) && isOpen(i, j)
			}
			.foldLeft(extremeIndex) { (acc, point) =>
				toIndex(point._1, point._2) :: acc
			}
	}

	private def isInBounds(i: Int, j: Int) = {
		i >= 0 && i < rows && j >= 0 && j < columns
	} 

	private def toIndex(i: Int, j: Int): Int = {
		columns * i + j
	}

}

object Percolation {
	def apply(width: Int, height: Int): Percolation = new Percolation {
		val rows: Int = width
		val columns: Int = height
	}
}