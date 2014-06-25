import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunPerculationStatistics extends FunSuite {

	test("run perculations test") {
		val test = PerculationStatistics(2, 10)

		assert(test.mean > 0 && test.mean < 2)
		assert(test.stddev > 0 && test.stddev < 2)
	}
}