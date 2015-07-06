package patternRecognition

object Recognition {

	def fast(points: List[Point]): List[List[Point]] = {

		def toId(first: Point, next: Point): String = {
			"" + first + next
		}

		def fetchPattern(index: Int, acc: Map[String, List[Point]]): Map[String, List[Point]] = {
			val items = points slice (index, points.length)
			val head = items.head

			items.tail
				.groupBy(head.slopeTo(_))
				.filter(_._2.size >= 3)
				.map(_._2)
				.foldLeft(acc){ (acc, group) =>
					val pair = (toId(head, group.head) -> (head :: group))
					val prevSegment = toId(group.head, group.tail.head)
					(
						if (acc isDefinedAt prevSegment)
							(acc - prevSegment)
						else
							acc
					) + pair
				}
		}

		(0 until points.length-3)
			.foldRight(Map[String, List[Point]]())(fetchPattern)
			.values.map(_.sortWith(_ < _)).toList

	}

	def brute(points: List[Point]): List[List[Point]] = {
		val length = points.length
		val items = for {
			i <- (0 until length)
			j <- (i+1 until length)
			k <- (j+1 until length)
			l <- (k+1 until length)
			pointsIJ = points(i) slopeTo points(j)
			pointsJK = points(j) slopeTo points(k)
			pointsKL = points(k) slopeTo points(l)
			pointsLI = points(l) slopeTo points(i)
			if (pointsIJ==pointsJK && pointsJK == pointsKL
				&& pointsKL==pointsLI)
		} yield List(points(i), points(j), points(k), points(l))
		items.toList
	}
}