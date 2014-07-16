package randomizedQueueAndDeque

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DequeTest extends FunSuite {

	test("Deque should be created from list of strings") {
		var deque = Deque("A", "B", "C")
		assert(deque.iterator.toList == List("A", "B", "C"))
	}

	test("Deque should add element to the beginning of queue") {
		var deque = Deque("B", "C")
		deque.addFirst("A")
		assert(deque.iterator.toList == List("A", "B", "C"))
	}

	test("Deque should add element to the end of queue") {
		var deque = Deque("A", "B")
		deque.addLast("C")
		assert(deque.iterator.toList == List("A", "B", "C"))
	}

	test("Deque should remove element from the beginning of queue") {
		var deque = Deque("Z", "A", "B", "C")
		assert(deque.removeFirst() == "Z", "Removed elemet should be Z")
		assert(deque.iterator.toList == List("A", "B", "C"))
	}

	test("Deque should remove element from the end of queue") {
		var deque = Deque("A", "B", "C", "D")
		assert(deque.removeLast() == "D", "Removed elemet should be D")
		assert(deque.iterator.toList == List("A", "B", "C"))
	}
}