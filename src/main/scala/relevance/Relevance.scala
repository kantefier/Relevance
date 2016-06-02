package relevance

import Metric._
import scala.io.{BufferedSource, Source}
import scala.util.Try
import scala.util.parsing.combinator._

// define some domain classes
case class User(ident: String, library: List[ArtistInfo])
object User {
	def parse(str: List[String]): List[User] = {
		str.groupBy(_.split(" ").head)
	}
}
case class ArtistInfo(name: String, mbid: Option[String], plays: Int)


object UserParser extends RegexParsers {
	def userString: Parser[User] = phrase(userId ~ opt(artistMbId) ~ artistName ~ playsCount) ^^ {
		case userIdent ~ artistMbIdOpt ~ artistNameStr ~ plays =>
			User(userIdent, ArtistInfo(artistNameStr, artistMbIdOpt, plays) :: Nil)
	}

	def userId: Parser[String] = """[0-9a-z]{40}""".r

	def artistMbId: Parser[String] = """[0-9a-z]{8}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{12}""".r

	def artistName: Parser[String] = repsep(artistNamePart, " ") ^^ {case nameParts => nameParts.mkString(" ")}
	def artistNamePart: Parser[String] = """[\w\p{L}]+""".r

	def playsCount: Parser[Int] = """\d+""".r ^^ {case intTerm => intTerm.toInt}
}

///////
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


	/// pure part
	def findMostRelevant[T](base: T, others: List[T])(implicit ev: Metric[T]): T = {
		//inner tail recursive function
		def innerIter(others: List[T], bestSoFar: T, bestMetric: Double): T = others match {
			case Nil => bestSoFar
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
			case x :: theRest => innerIter(theRest, x, ev.similarity(base, x))
		}
	}
}