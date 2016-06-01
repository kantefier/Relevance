package relevance

import Metric._

// define some domain classes
case class User(ident: String, library: List[ArtistInfo])
case class ArtistInfo(name: String, mbid: Option[String], plays: Int)

object Relevance {
	import scala.language.implicitConversions

	def main(args: Array[String]) = {
		val artists1 = ArtistInfo("qwe", None, 2) :: ArtistInfo("asd", None, 3) :: Nil
		val user1 = User("Charlie", artists1)
		val artists2 = ArtistInfo("qwe", None, 4) :: ArtistInfo("asd", None, 9) :: Nil
		val user2 = User("Tony", artists2)
		val artists3 = ArtistInfo("qwe", None, 4) :: ArtistInfo("asd", None, 6) :: Nil
		val user3 = User("Goby", artists3)

		val bestUser = findMostRelevant(user1, user2 :: user3 :: Nil)

		println(s"Most relevant for $user1 is $bestUser")

	}


	/// pure part
	def findMostRelevant[T](base: T, others: List[T])(implicit ev: Metric[T]): T = {
		//inner tail recursive function
		def innerIter(others: List[T], bestSoFar: T, bestMetric: Double): T = others match {
			case Nil => bestSoFar
			case current :: others =>
				val currentSim = ev.similarity(base, current)
				if(currentSim > bestMetric)
					innerIter(others, current, currentSim)
				else
					innerIter(others, bestSoFar, bestMetric)
		}

		others match {
			// if there's no others, the best match to a user is himself
			case Nil => base
			case x :: others => innerIter(others, x, ev.similarity(base, x))
		}
	}
}