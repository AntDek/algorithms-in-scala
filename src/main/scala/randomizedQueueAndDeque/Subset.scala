import randomizedQueueAndDeque.RandomizedQueue

/**
	Subset client.
	Write a client program Subset.java that takes a command-line integer k; 
	reads in a sequence of N strings from standard input using StdIn.readString(); 
	and prints out exactly k of them, uniformly at random. Each item from the sequence
	can be printed out at most once. You may assume that k ≥ 0 and no greater than 
	the number of string N on standard input.

	% echo A B C D E F G H I | java Subset 3       % echo AA BB BB BB BB BB CC CC | java Subset 8
	C                                              BB
	G                                              AA
	A                                              BB
	                                               CC
	% echo A B C D E F G H I | java Subset 3       BB
	E                                              BB
	F                                              CC
	G                                              BB
**/

object Subset extends App {
	val set = RandomizedQueue[String](StdIn.readAllStrings())
	// val count = args.head
	set take(2) foreach println
}