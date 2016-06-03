package relevance

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file._

case class ResultWriter(filePath: String) {
    val resultsFileChannel = FileChannel.
      open(Paths.get(filePath), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)

    /**
     *  Write a line to file
     */
    def write(resultString: String): Unit = {
        resultsFileChannel.
          write(ByteBuffer.wrap((resultString + "\n").getBytes("UTF-8")))
    }

    /**
     * Force writing to disk
     */
    def flush() = resultsFileChannel.force(true)
}
