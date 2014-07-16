import randomizedQueueAndDeque.RandomizedQueue

object Subset extends App {
	val set = RandomizedQueue[String](StdIn.readAllStrings())
	set take(2) foreach println
}