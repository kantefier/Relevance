package relevance

import scala.util.Random

/**
 * Testing data source
 */
object TestingData {
    lazy val rand = new Random(System.nanoTime())

    /**
     * Generate a random String of given length from given Chars
     * Example:
     * randomStringFromChars(('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9'), 100)
     *
     */
    def randomStringFromChars(chars: Seq[Char], length: Int): String = {
        (1 to length).map { _ =>
            chars(rand.nextInt(chars.length - 1))
        }.mkString
    }

    val testingArtists: List[String] = (1 to 10).map { _ =>
        randomStringFromChars(('a' to 'z') ++ ('0' to '9'), 20)
    }.toList

    /**
     * Fifry lines of testing data, same format as in the dataset
     */
    def testingFifty: List[String] = (1 to 10).flatMap { _ =>
        val userIdent = randomStringFromChars(('a' to 'z') ++ ('0' to '9'), 40)
        testingArtists.map { artist =>
            userIdent + " iywgeiu " + artist + " " + rand.nextInt(200)
        }
    }.toList
}
