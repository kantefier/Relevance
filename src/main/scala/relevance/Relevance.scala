package relevance

import relevance.model._
import scala.util.Try
import scala.language.implicitConversions

object Relevance {

	def main(args: Array[String]) = {
		val artists1 = ArtistInfo("qwe", None, 2) :: ArtistInfo("asd", None, 3) :: Nil
		val user1 = User("Charlie", artists1)
		val artists2 = ArtistInfo("qwe", None, 4) :: ArtistInfo("asd", None, 9) :: Nil
		val user2 = User("Tony", artists2)
		val artists3 = ArtistInfo("qwe", None, 4) :: ArtistInfo("asd", None, 6) :: Nil
		val user3 = User("Goby", artists3)
		val bestUser = findMostRelevant(user1, user2 :: user3 :: Nil)
		println(s"Most relevant for $user1 is $bestUser")

		/////

		import TestingData._

		val everyone = User.parse(testingFifty)
		everyone.foreach { currentUser =>
			val bestMatch = findMostRelevant(currentUser, everyone)
			println(s"Best match for ${currentUser.ident} is ${bestMatch.ident}\n")
		}

	}

	/*def sequentialPerform(datasetFilepath: String, currentUser: User): User = {
		for {
			line <- Source.fromFile(datasetFilepath).getLines()
		}
	}*/

	/**
	 *	Read a chunk of data from given iterator
	 */
	def getChunk(source: Iterator[String], minChunkSize: Int = 200): (List[String], Iterator[String]) = {
		// two strings are records for the same user if the first substrings before spaces are equal
		def sameUserStrings(str1: String, str2: String): Boolean = {
			val indexOfSpace1 = str1.indexOf(' ')
			val indexOfSpace2 = str2.indexOf(' ')
			Try(str1.substring(0, indexOfSpace1) == str2.substring(0, indexOfSpace2)).getOrElse(false)
		}

		// recursive iteration function
		def iterTaker(taken: List[String], prevIter: Iterator[String], chunksTaken: Int = 0): (List[String], Iterator[String]) = {
			if(source.hasNext) {
				val nextItem = source.next()

				if(chunksTaken < minChunkSize) {
					iterTaker(nextItem :: taken, prevIter.drop(1), chunksTaken + 1)
				} else if(sameUserStrings(nextItem, taken.headOption.getOrElse(""))) {
					iterTaker(nextItem :: taken, prevIter.drop(1), chunksTaken + 1)
				} else {
					(taken, prevIter)
				}
			} else {
				(taken, Iterator.empty)
			}
		}

		if(source.hasNext) {
			val prevIterator = source.duplicate._2
			iterTaker(source.next :: Nil, prevIterator)
		} else {
			(Nil, Iterator.empty)
		}
	}


	/**
	 *	Given an object and a list of objects that have an implicit Metric defined, return the best matching
	 */
	def findMostRelevant[T <: Named](base: T, others: List[T])(implicit ev: Metric[T]): T = {
		//inner tail recursive function
		def innerIter(others: List[T], bestSoFar: T, bestMetric: Double): T = others match {
			case Nil =>
				println(s"Best metric for $base was: $bestMetric")
				bestSoFar

			// skip comparing the object to itself
			case current :: theRest if current.name == base.name =>
				innerIter(theRest, bestSoFar, bestMetric)

			case current :: theRest =>
				val currentSim = ev.similarity(base, current)
				if(currentSim > bestMetric)
					innerIter(theRest, current, currentSim)
				else
					innerIter(theRest, bestSoFar, bestMetric)
		}

		others match {
			// if there's no others, the best match to a user is himself
			case Nil => base
			case x :: theRest =>
				val initialMetric = if(x.name != base.name) ev.similarity(base, x)
					else -1.0

				innerIter(theRest, x, initialMetric)
		}
	}
}