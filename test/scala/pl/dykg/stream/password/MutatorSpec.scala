package pl.dykg.stream.password

import org.specs2.mutable.Specification

import pl.dykg.stream.password.Mutator._



class MutatorSpec extends Specification {

  "casify mutator" should {
    "not be empty for not empty word with letters" in {
      swapCase("house") must not be empty
    }

    "be empty if word does not contain any letters" in {
      swapCase("$%^#") must be empty
    }

    "generate at least one word with changed case" in {
      swapCase("house") must contain("House", "HOuse", "HoUSe", "housE")
    }

    "not contain original word" in {
      swapCase("house") must not contain "house"
    }
  }

  "replacement mutator" should {
    "be empty if input word is empty" in {
      replacements("") must be empty
    }

    "be empty if input does not contain characters to be replaced" in {
      replacements("ggg") must be empty
    }

    "not contain original word" in {
      replacements("some") must not contain("some")
    }

    "contain word with changed e letter to 3" in {
      replacements("elite") must contain("3lite", "3lit3", "elit3", "el1te")
    }

    "contain word with changed a letter to @ or 4" in {
      replacements("alabama") must contain("4l@bama", "al@b@m@", "4l@b4m@")
    }
  }


  "flatmap" should {
    "work" in {
      Stream("car", "house").flatMap(word => swapCase(word)) must contain("CaR", "hOUse")
    }
  }

}
