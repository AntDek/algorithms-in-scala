import scala.collection.mutable.PriorityQueue
import java.util.Random

/**
	Monte Carlo simulation

	- Initialize N-by-N whole grid to be blocked.
	- Declare random sites open until top connected to bottom.
	- Vacancy percentage estimates p*.
**/
class PerculationStatistics (heap: PriorityQueue[Double]) {

	lazy val mean: Double = heap.sum / heap.size.toDouble

	lazy val stddev: Double = {
		math.sqrt( (0.0 /: heap) { (a,e) =>
			a + math.pow(e - mean, 2.0)
	  } / heap.size)
	}

	lazy val confidenceLo: Double = heap.last

	lazy val confidenceHi: Double = heap.head
}	

object PerculationStatistics {

	def apply(N: Int, T: Int): PerculationStatistics =
		new PerculationStatistics(makeHeap(N, T))

	private val rand = new Random

	private def makeHeap(N:Int, T:Int): PriorityQueue[Double] = {
		val heap = new PriorityQueue[Double]
		(0 until T).foreach(x=>heap += getTimes(N))
		heap
	}

	private def getTimes(N:Int): Double = {
		val pr = Percolation(N,N)
		var times = 0.0
		while (!pr.percolates()) {
			var i = rand.nextInt(N)
			var j = rand.nextInt(N)
			if (!pr.isOpen(i, j)) {
				pr.open(i, j)
				times += 1
			}
		}

		times/(N*N).toDouble
	}
}