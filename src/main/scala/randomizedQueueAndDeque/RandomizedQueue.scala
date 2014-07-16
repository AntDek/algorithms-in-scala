package randomizedQueueAndDeque

import java.util.Random
import scala.collection.TraversableOnce


trait RandomizedQueue[A] extends Iterable[A] {
	
	protected var queue: List[A]

	private val rand: Random = new Random
	
	def iterator: Iterator[A] = new Iterator[A] {
		private var indices = RandomizedQueue[Int](queue.indices)
		def hasNext = indices.isEmpty
		def next() = queue(indices.dequeue())
	}

	def enqueue(a: A): Unit = { 
		queue = a :: queue
	}

	def dequeue(): A = {
		val (head, tail) = queue splitAt getRandom
		queue = head ::: tail.tail
		tail.head
	}

	def sample(): A = queue(getRandom)

	override def isEmpty = queue.length > 0

	override def size = queue.length

	private def getRandom: Int = rand nextInt queue.length
}

object RandomizedQueue {

	def apply[A](xs: A*) = {
		new RandomizedQueue[A] {
			var queue: List[A] = xs.toList
		}
	}

	def apply[A](xs: TraversableOnce[A]) = {
		new RandomizedQueue[A] {
			var queue: List[A] = xs.toList
		}
	}

	def apply[A] = {
		new RandomizedQueue[A] {
			var queue: List[A] = List[A]()
		}
	}
}