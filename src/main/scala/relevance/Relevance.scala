package relevance

import relevance.model._
import scala.util.Try
import scala.io.{Codec, Source}
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

		//////

		println("Mega test begins!\n")
		rootProcess()
/*
		println("Same shit, but straight forward")
		val users = User.parse(Source.fromFile("D:\\Dev\\dataset\\hundred")(Codec.UTF8).getLines().toList)
		users.map{user =>
			user -> findMostRelevant(user, users)
		}.foreach(pair => println(pair._1 + " " + pair._2))*/
	}

	def performSequentialRead[T](sourceIter: Iterator[String], resultAccum: List[T] = Nil)(f: List[User] => T): List[T] =
		if(sourceIter.hasNext) {
			val (dataChunk, nextChunkIterator) = getChunk(sourceIter, 50)
			val users = User.parse(dataChunk)
			val currentResult = f(users)
			performSequentialRead(nextChunkIterator, currentResult :: resultAccum)(f)
		} else {
			resultAccum
		}

	def rootProcess(): Unit = {
		val datasetFilePath = "D:\\Dev\\dataset\\usersha1-artmbid-artname-plays.tsv"
//		val datasetFilePath = "D:\\Dev\\dataset\\hundred"

		val mainSourceIterator = Source.fromFile(datasetFilePath)(Codec.UTF8).getLines()

		performSequentialRead(mainSourceIterator) { parsedUsers =>
			parsedUsers.par.map { currentUser =>
				val secondSourceIter = Source.fromFile(datasetFilePath)(Codec.UTF8).getLines()
				val relevantVariants = performSequentialRead(secondSourceIter) { others =>
					findMostRelevant(currentUser, others)
				}
				currentUser.toString + " " + findMostRelevant(currentUser, relevantVariants).toString
			}.toList
		}.flatten.foreach(println)
		/*val data = mainSourceIterator.toList
		val userList = User.parse(data)
		userList.foreach { currentUser =>
			val bestMatch = findMostRelevant(currentUser, userList)
//			println(s"Best match for $currentUser is $bestMatch")
		}*/
	}

	/**
	 *	Read a chunk of data from given iterator
	 */
	def getChunk(inputIter: Iterator[String], minChunkSize: Int = 500): (List[String], Iterator[String]) = {
		// two strings are records for the same user if the first substrings before spaces are equal
		def sameUserStrings(str1: String, str2: String): Boolean = str1 -> str2 match {
			case (User.singleUserLine(user1, _), User.singleUserLine(user2, _)) => user1 == user2
			case _ => false
		}

		val source = inputIter.buffered
		// recursive iteration function
		def iterTaker(taken: List[String], chunksTaken: Int = 0): (List[String], Iterator[String]) = {
			if(source.hasNext) {
				source.head match {
					case str if (chunksTaken < minChunkSize) | sameUserStrings(str, taken.headOption.getOrElse("")) =>
						iterTaker(source.next :: taken, chunksTaken + 1)
					case _ =>
						(taken, source)
				}
			} else {
				(taken, Iterator.empty)
			}
		}

		iterTaker(Nil)
	}


	/**
	 *	Given an object and a list of objects that have an implicit Metric defined, return the best matching
	 */
	def findMostRelevant[T <: Named](base: T, others: List[T])(implicit ev: Metric[T]): T = {
		//inner tail recursive function
		def innerIter(others: List[T], bestSoFar: T, bestMetric: Double): T = others match {
			case Nil =>
//				println(s"Best metric for $base was: $bestMetric")
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