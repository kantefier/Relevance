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

    def genArtists(count: Int): List[String] = (1 to count).map {_ =>
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

    /**
    *   Generate lines of testing data for given user count and artist count
    */
    def testingUserData(userCount: Int, artistCount: Int): List[String] = {
        val artists: List[String] = genArtists(artistCount)
        (1 to userCount).flatMap { _ =>
            val userIdent = randomStringFromChars(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'), 40)
            artists.map { artist =>
                userIdent + " qwertyMBIDqwerty " + artist + " " + rand.nextInt(2000)
            }
        }
    }.toList

    /**
    *   Generates infinite testing data stream
    */
    def infiniteData(artistCount: Int): Stream[String] = {
        val artists: List[String] = genArtists(artistCount)
        Stream.from(1).flatMap { _ =>
            val userIdent = randomStringFromChars(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'), 40)
            artists.map { artist =>
                userIdent + " qwertyMBIDqwerty " + artist + " " + rand.nextInt(2000)
            }
        }
    }

    /**
    *   Writes infinite stream to file
    */
    def streamToFile(stream: Stream[String], filePath: String, maxLines: Int, chunkSize: Int = 50): Unit = {
        import java.io._

        val outFile = {
            val daFile = new File(filePath)
            daFile.createNewFile()
            daFile
        }
        val outStream = new BufferedOutputStream( new FileOutputStream(outFile, false) )

        def performWrite(inStream: Stream[String], currentLine: Int): Unit = {
            if(inStream.isEmpty || currentLine >= maxLines) {
                Unit //return from recursion
            } else {
                val thisChunkSize = {
                    if(currentLine + chunkSize > maxLines)
                        maxLines - currentLine
                    else
                        chunkSize
                }
                val (streamChunk, restStream) = inStream.splitAt(thisChunkSize)
                streamChunk.map(str => (str + "\n").getBytes("UTF-8")).foreach(outStream.write)
                performWrite(restStream, currentLine + thisChunkSize)
            }
        }

        try performWrite(stream, 0) finally outStream.close
    }
}
