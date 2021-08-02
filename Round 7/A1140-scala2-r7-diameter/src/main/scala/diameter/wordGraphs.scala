// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package diameter

object wordGraphs {
  val words3 = List("PEZ", "run", "ran", "urn", "LAN", "dog", "cat", "pie", "may", "day", "aaa", "abb", "odd", "the", "aby", "van", "dat", "die", "sky", "can", "FTP", "one", "two", "few", "bit", "sun", "man", "are", "cum", "cod", "six", "lol", "bot", "ten", "tao", "war", "BBC", "and", "now", "tax", "big", "but", "say", "add", "SMS", "SIM", "new", "gas", "bio", "ace", "map", "gag", "ask", "win", "cow", "yes", "buy", "red", "yet", "see", "age", "pip", "pit", "sol", "ICQ", "ass", "too", "toe", "for", "yak", "uey", "ate", "boy", "his", "her", "pom", "qua", "air", "all", "ant", "any", "arm", "art", "was", "wax", "son", "bee", "non", "car", "hot", "not", "men", "rue", "how", "dye", "rat", "tan", "lob", "hut", "pen", "tin", "gun", "gay", "ray", "egg", "dub", "del", "god", "bad", "bam", "ban", "bap", "bat", "bed", "cry", "eye", "zak", "beg", "bel", "ben", "ear", "leg", "axe", "hay", "ail", "law", "lam", "pin", "oil", "foe", "ilk", "bet", "bid", "cut", "dig", "dug", "did", "eat", "fed", "fit", "fly", "get", "got", "had", "hid", "hit", "lay", "led", "let", "lie", "lit", "met", "bog", "rod", "mal", "sic", "oar", "bug", "why", "mow", "bra", "sin", "bag", "pay", "put", "rid", "run", "ran", "saw", "set", "sew", "sit", "sat", "wed", "wet", "won", "era", "bib", "sot", "box", "act", "ven", "ice", "dry", "end", "jay", "tit", "pig", "fox", "ram", "nit", "rim", "Rom", "sap", "low", "far", "old", "fog", "ash", "wow", "lil", "sal", "key", "Jew", "nut", "dew", "ewe", "pet", "ebb", "ore", "gem", "kin", "yon", "pub", "mop", "hem", "jeg", "han", "hun", "ile", "wee", "cwm", "las", "hey", "ABC", "aha", "sea", "ham", "jag", "due", "cop", "rot", "sir", "dud", "elf", "eve", "nil", "obi", "duo", "moo", "cor", "con", "eta", "XXX", "dag", "dam", "dal", "wok", "fag", "bus", "sex", "emu", "pot", "nah", "tar", "vat", "pan", "wan", "gut", "rho", "jet", "alb", "own", "pop", "way", "bow", "ply", "net", "jib", "flu", "gnu", "bar", "zoo", "pun", "pat", "tap", "vid", "tau", "phi", "chi", "psi", "fan", "lad", "Lao", "wat", "tie", "ohm", "lux", "kim", "gym", "eel", "ach", "ser", "arc", "fat", "lip", "ape", "tea", "cos", "don", "awe", "dis", "yaw", "jaw", "paw", "cub", "yam", "mug", "tod", "cot", "ink", "raw", "poo", "log", "lug", "aal", "rex", "nix", "ska", "row", "Luo", "gab", "loo", "sum", "wen", "joy", "via", "lex", "top", "wag", "ton", "neu", "rad", "rob", "doh", "fah", "soh", "lah", "nee", "eth", "job", "per", "edh", "lid", "gar", "sow", "cob", "hop", "tom", "tup", "baa", "wit", "hen", "doe", "kip", "foo", "rub", "cup", "pee", "pea", "peg", "gig", "gel", "jam", "fop", "Noh", "kid", "tor", "ren", "ken", "bok", "den", "tik", "eet", "som", "zit", "out", "neg", "imp", "ado", "oca", "fap", "caw", "aft", "ago", "aid", "aim", "nun", "ale", "rip", "doo", "yeh", "fun", "yin", "tow", "ski", "fee", "use", "tee", "gum", "bob", "sai", "roe", "uni", "hat", "sad", "nap", "REM", "dos", "nor", "xor", "ono", "Joe", "ish", "orb", "bum", "pap", "hoe", "Zug", "fry", "dir", "oak", "cap", "pro", "apt", "ufo", "ipe", "fin", "off", "sub", "hic", "fix", "cab", "dad", "ala", "kit", "try", "bun", "gib", "med", "yay", "has", "nay", "keg", "zee", "ego", "zed", "web", "zap", "veg", "ord", "din", "ire", "cad", "dab", "lab", "fab", "fax", "abo", "aah", "aba", "ess", "fad", "jab", "nab", "mac", "lez", "les", "yob", "kea", "wai", "mel", "inn", "fer", "sue", "cru", "kot", "mar", "Jap", "nip", "ode", "olf", "omy", "pad", "par", "mad", "pow", "roc", "azo", "aye", "awk", "awl", "awn", "ave", "mag", "sac", "zen", "sup", "fem", "tia", "fur", "pes", "pal", "mat", "yew", "bro", "dot", "yap", "fen", "orc", "fez", "pod", "zho", "dzo", "toy", "moi", "tot", "tag", "vum", "ill", "bay", "hip", "baz", "bin", "rye", "fig", "jar", "bud", "pus", "tun", "wey", "tey", "moa", "teh", "gad", "eff", "col", "WAN", "eke", "vet", "rap", "eek", "wig", "reg", "lei", "res", "yea", "mix", "ion", "guy", "leu", "ark", "yen", "lag", "spy", "hir", "wif", "wid", "gob", "coz", "pic", "sax", "SVO", "sov", "lav", "gee", "haw", "emo", "wry", "boo", "ref", "alp", "amp", "ana", "auk", "bey", "boa", "bub", "cam", "cog", "cox", "cud", "cue", "cur", "dey", "dor", "dow", "dun", "eft", "ell", "erg", "fay", "fib", "fob", "fro", "gam", "gyp", "hap", "hex", "hob", "hod", "hoy", "hub", "hue", "jig", "jot", "kay", "lac", "lar", "lea", "lop", "lye", "maw", "mew", "mir", "mod", "nan", "nib", "nob", "oaf", "oft", "pas", "pep", "pew", "poi", "pol", "pug", "raj", "reb", "rep", "ret", "rut", "sen", "sib", "sod", "sop", "sou", "soy", "tad", "tas", "tat", "tic", "tog", "vee", "vie", "vis", "woe", "yip", "zag", "zig", "gap", "hag", "hep", "hie", "lap", "lot", "nag", "neb", "pry", "rag", "ree", "sag", "shy", "sly", "sty", "vow", "yep", "fir", "cis", "tut", "Kop", "tux", "eon", "PTA", "esh", "pec", "bac", "git", "tip", "SOS", "vav", "waw", "vau", "kaf", "mem", "tav", "qat", "kat", "TMI", "gat", "owe", "ned", "irk", "mud", "mon", "ide", "ley", "RSA", "woo", "dim", "hum", "sip", "nod", "ivy", "rum", "meh", "hug", "spa", "bye", "dip", "dau", "deg", "mil", "mum", "jun", "jug", "taa", "dap", "doc", "exy", "uzi", "bov", "roo", "ute", "zoe", "lox", "mia", "rig", "lai", "gin", "jpg", "vex", "mid", "vac", "org", "pov", "SAR", "alt", "rem", "nid", "meu", "min", "pud", "lee", "zip", "gal", "rev", "jog", "sig", "coy", "opt", "FOB", "err", "elm", "wad", "wem", "yah", "mob", "dop", "ard", "urn", "lek", "fud", "tug", "ugh", "tab", "nth", "coo", "mux", "CKs", "yup", "mom", "afa", "zax", "vas", "dit", "dah", "ope", "vol", "goo", "pix", "ama", "lax", "bod", "rib", "DUI", "com", "dib", "hew", "taj", "asp", "abs", "ait", "def", "cay", "tel", "dux", "tib", "Arm", "Bug", "Del", "Dom", "Don", "Edo", "Ewe", "Guy", "Ich", "Jag", "Jay", "Job", "Lot", "Mac", "Mal", "Ned", "Nip", "Rob", "Set", "Sol", "Sue", "Tor", "Tux", "Wed", "rog", "sam", "jew", "dee", "meg", "hog", "kir", "ohi", "fug", "eid", "ghi", "gau", "ahi", "DMC", "imu", "fey", "ria", "otp", "Gen", "pox", "int", "num", "icy", "sci", "bim", "har", "nog", "Aar", "AND", "tub", "Evo", "Urd", "sed", "cig", "tum", "mic", "gan", "vai", "hon", "Hon", "EMI", "rug", "bop", "deb", "fet", "vom", "ump", "sox", "esp", "LDS", "sss", "koi", "daw", "Boi", "gen", "ins", "zuz", "fid", "isu", "erk", "duh", "gog", "zib", "mas", "jut", "pav", "wiz", "pwn", "oik", "goy", "bak", "owt", "ova", "cep", "yar", "wop", "pup", "MOT", "sob", "Kos", "aul", "vog", "ety", "dup", "kif", "gaw", "khu", "oka", "ort", "luv", "XXI", "ake", "rez", "hor", "ooh", "ups", "sis", "yuk", "rah", "gon", "pyx", "kaw", "biz", "rec", "dol", "rax", "ens", "sim", "XOR", "NOR", "nen", "zek", "dob", "hov", "sny", "kor", "teg", "MiG", "gom", "mog", "mho", "eep", "yex", "BHA", "lev", "div", "ume", "oxo", "kut", "ELL", "sav", "wot", "jer", "Fon", "kef", "mam", "oxy", "adz", "ayr", "GRI", "soz", "eld", "ssh", "ket", "aad", "olm", "nub", "LXX", "guv", "zun", "naa", "yus", "yem", "ent", "Ayr", "ads", "DJs", "BBs", "lbs", "nim", "ais", "MRB", "hed", "pye", "gry")
  val words3common = List("the", "and", "for", "are", "but", "not", "you", "all", "any", "can", "had", "her", "was", "one", "our", "out", "day", "get", "has", "him", "his", "how", "man", "new", "now", "old", "see", "two", "way", "who", "boy", "did", "its", "let", "put", "say", "she", "too", "use")

  /**
   * Returns true iff the strings are of the same length and differ in exactly one position (in case insensitive mode).
   */
  private def areNeighbours(w1: String, w2: String): Boolean = {
    if (w1.length != w2.length)
      false
    else
      (w1 zip w2).count({case (c1, c2) => (c1.toLower != c2.toLower)}) == 1
  }

  /**
   * Given a sequence of strings of the same length, builds an undirected graph such that
   * - the vertices are the strings in the sequence, and
   * - there is an edge between two vertices (strings) if and only if the strings differ only in one location.
   */
  def getGraph(words: List[String]): (Graph, Map[String, Int]) = {
    require(words.size > 0, "The word list must not be empty")
    require(words.forall(w => w.length == words.head.length),
            "The words should be of the same length")
    val wordToIndex = words.zipWithIndex.toMap
    val edges = new scala.collection.mutable.HashSet[(Int,Int)]
    var l1 = words
    while (l1.nonEmpty) {
      val w1 = l1.head
      for (w2 <- l1.tail) {
        if (areNeighbours(w1, w2))
          edges((wordToIndex(w1), wordToIndex(w2))) = true
      }
      l1 = l1.tail
    }
    (new Graph(words.length, edges.toSeq), wordToIndex)
  }
}
