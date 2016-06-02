package relevance

import scala.util.Random

object TestingData {
  lazy val rand = new Random(System.nanoTime())

  def randomStringFromChars(chars: Seq[Char], length: Int): String = {
    (1 to length).map{_ =>
      chars(rand.nextInt(chars.length - 1))
    }.mkString
  }

  val testingArtists: List[String] = (1 to 10).map{ _ =>
    randomStringFromChars(('a' to 'z') ++ ('0' to '9'), 20)
  }.toList

  def testingFifty: List[String] = (1 to 10).flatMap {_ =>
    val userIdent = randomStringFromChars(('a' to 'z') ++ ('0' to '9'), 40)
    testingArtists.map { artist =>
       userIdent + " iywgeiu " + artist + " " + rand.nextInt(200)
    }
  }.toList
}
