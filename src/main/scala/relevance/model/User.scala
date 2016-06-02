package relevance.model

case class User(ident: String, library: List[ArtistInfo]) extends Named {
    val name = ident

    override def toString: String = ident
}

object User {
    private val userId = "[0-9a-z]{40}"
    private val singleUserLine = raw"""($userId)\s+(.*)""".r

    /**
     * Parse multiple Users
     */
    def parse(inputLst: List[String]): List[User] = inputLst.
        groupBy {
        case singleUserLine(ident, rest) => ident
    }.map {
        case (userIdent, userRecords) =>
            val userLibrary = userRecords.map { record =>
                // cut out user ident from the beginning
                val recordTrimmed = record.dropWhile(_ != ' ').trim
                ArtistInfo.parse(recordTrimmed)
            }
            User(userIdent, userLibrary)
    }.toList
}