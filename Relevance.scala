object Relevance {
	def main(args: Array[String]) = {
		println("Hello world!")
	}

	///
	case class User(ident: String, library: List[ArtistInfo])
	case class ArtistInfo(name: String, mbid: Option[String], plays: Int)

	/// pure part
	def similarity(usr1: User, usr2: User): Int = {
		// общие артисты из списка первого пользователя
		val commonArtists1 = usr1.library.filter(artInfo => usr2.library.map(_.name).contains(artInfo.name))
		// общие артисты из списка второго пользователя
		val commonArtists2 = usr2.library.filter(artInfo => usr1.library.map(_.name).contains(artInfo.name))

	}
}