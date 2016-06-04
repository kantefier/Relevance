package relevance.model

import scala.annotation.implicitNotFound
@implicitNotFound("No member of type class Metric in scope for ${T}")
trait Metric[T] {
	def similarity(x: T, y: T): Double
}

object Metric {
	implicit object MetricForUser extends Metric[User] {
		// use Pearson's metric to measure the "distance" between two user's musical tastes
		def similarity(x: User, y: User): Double = {
			// collect common artists from users' listening libraries
			val commonArtistsX: List[ArtistInfo] = x.library.filter(artInfo => y.library.map(_.name).contains(artInfo.name))
			val commonArtistsY: List[ArtistInfo] = y.library.filter(artInfo => x.library.map(_.name).contains(artInfo.name))

			// common artists count
			val N = commonArtistsX.length.toDouble
			if(N <= 2)
				0.0
			else {
				// sums of plays
				val sumX = commonArtistsX.map(_.plays).sum
				val sumY = commonArtistsY.map(_.plays).sum

				// sums of squared plays
				val sumSquareX = commonArtistsX.map(artist => artist.plays * artist.plays).sum
				val sumSquareY = commonArtistsY.map(artist => artist.plays * artist.plays).sum

				// sum of product of plays for corresponding artists
				val productSum = commonArtistsX.sortBy(_.name).zip(commonArtistsY.sortBy(_.name)).map {
					case (artistX, artistY) => artistX.plays * artistY.plays
				}.sum


				val numerator: Double = productSum - (sumX * sumY)/N
				val denominator: Double = scala.math.sqrt( (sumSquareX - (sumX * sumX)/N)*(sumSquareY - (sumY * sumY)/N) )

				if(denominator == 0.0)
					0.0
				else
					numerator / denominator
			}
		}
	}
}