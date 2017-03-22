object Anagrams {

    /** A word is simply a `String`. */
    type Word = String

    /** A sentence is a `List` of words. */
    type Sentence = List[Word]

    /** `Occurrences` is a `List` of pairs of characters and positive integers saying
      * how often the character appears.
      * This list is sorted alphabetically w.r.t. to the character in each pair.
      * All characters in the occurrence list are lowercase.
      *
      * Any list of pairs of lowercase characters and their frequency which is not sorted
      * is **not** an occurrence list.
      *
      * Note: If the frequency of some character is zero, then that character should not be
      * in the list.
      */
    type Occurrences = List[(Char, Int)]

    //def wordOccurrences(w: Word): Occurrences = w map (c => c.toLower) groupBy(c => w.count(ch => ch == c)) flatMap {case (count,let) => let map (c => c->count)} toList

    val word = "Robert"

    word.map(ch=> ch.toLower)
      .groupBy(ch => ch)
      .map{case (ch,str) => ch -> str.length}
      .toList
      .sorted

    //wordOccurrences(word)


    def combinations(occurrences: Occurrences): List[Occurrences] = {
            occurrences match {
                case Nil => List(List())
                case head::tail =>
                    val (char,count) = head
                    (for {
                        i <- 0 to count
                        occ <- combinations(tail)
                    }yield (char,i ) :: occ).toList map (list => list filter (_._2 != 0))
            }
        }

    combinations(List(('a', 2), ('b', 2)))




}
