package pl.dykg.stream.password

import pl.dykg.stream.password.Utils._

object Mutator {
  def swapCase(originalWord: String, index:Int = 0): Stream[String] = {
    if (index >= originalWord.length) {
      Stream.empty[String]
    } else if (originalWord.charAt(index).isLetter) {
      val changedWord = originalWord.switchCaseAt(index)
      changedWord #:: swapCase(changedWord, index + 1) ++ swapCase(originalWord, index+1)
    } else {
      swapCase(originalWord, index + 1)
    }
  }

  private val typicalReplacements = Map(
    'a' -> Seq('@', '4'),
    'i' -> Seq('!', '1'),
    'o' -> Seq('0'),
    's' -> Seq('5'),
    'z' -> Seq('2'),
    'e' -> Seq('3')
  )

  def replacements(originalWord: String, index: Int = 0): Stream[String] = {
    if (index >= originalWord.length) {
      Stream.empty[String]
    } else {
      val originalCharacter = originalWord.charAt(index).toLower
      val streamWithUnchangedLetter = replacements(originalWord, index + 1)
      val streamsWithChanges = typicalReplacements.getOrElse(originalCharacter, Nil).map((newCharacter: Char) => {
        val changedWord = originalWord.replaceCharAt(index, newCharacter)
        changedWord #:: replacements(changedWord, index + 1)}
      )
      mergeStreams(streamsWithChanges :+ streamWithUnchangedLetter)
    }
  }

  private def mergeStreams[A](streams: Seq[Stream[A]]): Stream[A] = streams match {
    case Nil => Stream.empty
    case first:: rest => first ++ mergeStreams(rest)
  }

}



