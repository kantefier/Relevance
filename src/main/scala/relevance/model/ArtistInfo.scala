package relevance.model

case class ArtistInfo(name: String, mbid: Option[String], plays: Int)

object ArtistInfo {
    /**
     * Artist MusicBrainz ID RegExp
     */
    private val artistMbId = "[0-9a-z]{8}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{12}"
    private val artistWithMbid = raw"""($artistMbId)\s+(.*)""".r

    private def parseNameAndPlays(input: String): (String, Int) = {
        val tokenized = input.split("\\s")
        tokenized.init.mkString(" ").trim -> tokenized.last.toInt
    }

    /**
     * Parse one line of artist info (for single user)
     */
    def parse(input: String): ArtistInfo = input match {
        case artistWithMbid(mbid, restString) =>
            val (artistName, plays) = parseNameAndPlays(restString)
            ArtistInfo(artistName, Some(mbid), plays)
        case artistWithoutMbid =>
            val (artistName, plays) = parseNameAndPlays(artistWithoutMbid)
            ArtistInfo(artistName, None, plays)
    }
}