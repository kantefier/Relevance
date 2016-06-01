object Relevance {
	def main(args: Array[String]) = {
		println("Hello world!")
	}

	///
	// define some domain classes
	case class User(ident: String, library: List[ArtistInfo])
	case class ArtistInfo(name: String, mbid: Option[String], plays: Int)
	///

	trait Metric[T] {
		def similarity(x: T, y: T): Double
	}
	implicit object MetricForUser extends Metric[User] {
		// use Pearson's metric to measure the "distance" between two user's musical tastes
		def similarity(x: User, y: User): Double = {
			// collect common artists from users' listening libraries
			val commonArtistsX = x.library.filter(artInfo => y.library.map(_.name).contains(artInfo.name))
			val commonArtistsY = y.library.filter(artInfo => commonArtistsX.map(_.name).contains(artInfo.name))
			??? TODO
		}
	}

	///

	/// pure part
	def findMostRelevant[T : Metric](base: T, others: List[T]): T = {
		//inner tail recursive function
		def innerIter(others: List[T], bestSoFar: T, bestMetric: Double): T = others match {
			case Nil => bestSoFar
			case current :: others =>
				val currentSim = similarity(base, current)
				if(currentSim > bestMetric)
					innerIter(others, current, currentSim)
				else
					innerIter(others, bestSoFar, bestMetric)
		}

		others match {
			// if there's no others, the best match to a user is himself
			case Nil => base
			case x :: others => innerIter(others, x, similarity(base, x))
		}
	}
}