package relevance.model

case class User(ident: String, library: List[ArtistInfo]) extends Named {
    val name = ident

    override def toString: String = ident
}

object User {
    private val userId = "[0-9a-z]{40}"
    val singleUserLine = raw"""($userId)\s+(.*)""".r

    /**
     * Parse multiple Users
     */
    def parse(inputLst: List[String]): List[User] = inputLst.
        groupBy {
        case singleUserLine(ident, rest) => ident
    }.map {
        case (userIdent, userRecords) =>
            val userLibrary = userRecords.map {
                case singleUserLine(_, restRecord) =>
                    // cut out user ident from the beginning
                    ArtistInfo.parse(restRecord.trim)
            }
            User(userIdent, userLibrary)
    }.toList
}