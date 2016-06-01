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
			val commonArtistsX: List[ArtistInfo] = x.library.filter(artInfo => y.library.map(_.name).contains(artInfo.name))
			val commonArtistsY: List[ArtistInfo] = y.library.filter(artInfo => commonArtistsX.map(_.name).contains(artInfo.name))

			// common artists count
			val N = commonArtistsX.length
			// TODO: return 0 if N == 0

			val sumX = commonArtistsX.map(_.plays).sum
			val sumY = commonArtistsY.map(_.plays).sum

			// TODO
			0.0
		}
	}

	///

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