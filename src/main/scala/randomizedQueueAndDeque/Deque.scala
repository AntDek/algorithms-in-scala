package randomizedQueueAndDeque

trait Deque[A] extends Iterable[A] {

	protected var deque: Vector[A]
	
	def iterator: Iterator[A] = deque.iterator
	
	def addFirst(a: A): Unit = { 
		deque = a +: deque
	}
	
	def addLast(a: A): Unit = {
		deque = deque :+ a
	}
	
	def removeFirst(): A = {
		val (head, tail) = (deque.head, deque.tail)
		deque = tail
		head
	}
	
	def removeLast(): A = {
		var (last, init) = (deque.last, deque.init)
		deque = init
		last
	}
}

object Deque {

	def apply[A](xs: A*) = {
		new Deque[A] {
			var deque: Vector[A] = xs.toVector
		}
	}

	def apply[A] = {
		new Deque[A] {
			var deque: Vector[A] = Vector[A]()
		}
	}
}