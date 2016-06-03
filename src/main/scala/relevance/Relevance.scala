package relevance

import relevance.model._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.{Codec, Source}
import scala.language.implicitConversions

object Relevance {

	/**
	 *	Entry point. Called on sbt's "run" task
	 */
	def main(args: Array[String]) = args.toList match {
		case inPath :: outPath :: mainChunkSize :: innerChunkSize :: Nil =>
			processDataset(inPath, outPath, mainChunkSize.toInt, innerChunkSize.toInt)
		case inPath :: outPath :: mainChunkSize :: Nil =>
			processDataset(inPath, outPath, mainChunkSize.toInt)
		case inPath :: outPath :: Nil =>
			processDataset(inPath, outPath)
		case _ =>
			println("""Usage:
  |first arg: dataset filepath
  |second arg: output filepath
  |[optional] third arg: main chunk size
  |[optional] fourth arg: secondary chunk size""".stripMargin)
	}

	/**
	 *	General function that launches all the calculations
	 */
	def processDataset(datasetFilePath: String, resultFilePath: String, mainChunkSize: Int = 500, secondaryChunkSize: Int = 1000): Unit = {
		import scala.concurrent.ExecutionContext.Implicits.global

		val mainSourceIterator = Source.fromFile(datasetFilePath)(Codec.UTF8).getLines()
		val resultWriter = ResultWriter(resultFilePath)

		performSequentialRead(mainSourceIterator, chunkSize = mainChunkSize) { parsedUsers =>
			parsedUsers.map { currentUser =>
				Future {

					val secondSourceIter = Source.fromFile(datasetFilePath)(Codec.UTF8).getLines()
					val relevantVariants: List[User] = performSequentialRead(secondSourceIter, chunkSize = secondaryChunkSize) { others =>
						Future(findMostRelevant(currentUser, others))
					}.map(futureUser => Await.result(futureUser, 5.minutes))

					currentUser.toString + " " + findMostRelevant(currentUser, relevantVariants).toString

				}.map(resultWriter.write)
			}.foreach(future => Await.ready(future, 10.minutes))
			resultWriter.flush()
		}
	}

	/**
	 *	Given Iterator[String], perform reading file by chunks, applying f to each chunk and accumulating the result
	 *	Returns a list of results
	 */
	def performSequentialRead[T](sourceIter: Iterator[String], resultAccum: List[T] = Nil, chunkSize: Int = 500)(f: List[User] => T): List[T] =
		if(sourceIter.hasNext) {
			val (dataChunk, nextChunkIterator) = getChunk(sourceIter, chunkSize)
			val users = User.parse(dataChunk)
			val currentResult = f(users)
			performSequentialRead(nextChunkIterator, currentResult :: resultAccum, chunkSize)(f)
		} else {
			resultAccum
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