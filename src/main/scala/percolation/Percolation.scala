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

	private lazy val sites: Array[Boolean] = new Array(rows*columns)

	private lazy val unionUF: WeightedQuickUnionUF = new WeightedQuickUnionUF(rows*columns)

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

	def percolates(): Boolean = {
		getTopSites exists (topIndex =>
			getButtomSites exists (buttomIndex => 
				unionUF connected (topIndex, buttomIndex)
			)
		)
	}

	private def findOpened(i: Int, j: Int): List[Int] = {
		val positions = List((i-1, j), (i+1, j), (i, j-1), (i, j+1))
		positions filter { case (i, j) =>
			isInBounds(i, j) && isOpen(i, j)
		} map { case (i, j) =>
			toIndex(i, j)
		}
	}

	private def isInBounds(i: Int, j: Int) = {
		i >= 0 && i < rows && j >= 0 && j < columns
	} 

	private def toIndex(i: Int, j: Int): Int = {
		columns * i + j
	}

	private def getTopSites(): List[Int] = {
		(0 until columns).toList filter (index => sites(index))
	}

	private def getButtomSites(): List[Int] = {
		val start = columns * (rows - 1)
		(start until start + columns).toList filter (index => sites(index))
	}

}

object Percolation {
	def apply(width: Int, height: Int): Percolation = new Percolation {
		val rows: Int = width
		val columns: Int = height
	}
}