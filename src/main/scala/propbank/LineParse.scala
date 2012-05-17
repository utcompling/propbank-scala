package srl.propbank
import scala.util.parsing.combinator._

class LineParserComb extends JavaTokenParsers {

  /**
   * Applies parse(line).
   */
  def apply(line:String):Option[LineRep] = parse(line)

  /**
   * Runs the parser on an instance.
   * @param parsers A LineParserComb instance.
   * @param line A line to be parsed.
   * @return Optionally, a parsed LineRep instance.
   */
  def parse(line:String):Option[LineRep] = {
    parseAll(lineRep, line) match {
      case Success(result, _) => Some(result)
      case _ => None
    }
  }

  //---the whole line---
  def lineRep:Parser[LineRep] = file ~ sentence ~ verbTerminal ~ tagger ~ frameSet ~ inflection ~ labels ^^ {
    case f ~ s ~ term ~ tgr ~ fs ~ i ~ labels => new LineRep(f, s, term, tgr, fs, i, labels)
  }
  def word = """\w+""".r
  def path = """\S+""".r 
  def file = path
  def sentence = decimalNumber ^^ (_ toInt)
  def verbTerminal = decimalNumber ^^ (_ toInt)
  def tagger = word
  def frameSet = word ~ ("." ~> decimalNumber) ^^ {
    case w ~ num => FrameSet(w, num.toInt)
  }
  def labels = rep(label)

  //---inflection---
  def inflection:Parser[Inflection] = form ~ tense ~ aspect ~ person ~ voice ^^ {
    case f ~ t ~ a ~ p ~ v => new Inflection(f, t, a, p, v)
  }

  private implicit def string2Regex(c:String) = c.r
  def form = "i" ^^^ Infinitive | "g" ^^^ Gerund | "p" ^^^ Participle | "v" ^^^ Finite | "-" ^^^ NoForm
  def tense = "f" ^^^ Future | "p" ^^^ Past | "n" ^^^ Present | "-" ^^^ NoTense
  def aspect = "p" ^^^ Perfect | "o" ^^^ Progressive | "b" ^^^ Both | "-" ^^^ NoAspect
  def person = "3" ^^^ Sing3rd | "-" ^^^ NoPerson
  def voice = "a" ^^^ Active | "p" ^^^ Passive | "-" ^^^ NoVoice

  //---labels ---
  def label = (relation <~ "-") ~ argLabel ^^ {
    case rel ~ arg => Label(rel, arg)
  }

  //relations are complex!
  def relation = chain
  def terminal = (decimalNumber <~ ":") ~ decimalNumber ^^ {
    case stn ~ sh => new Terminal(stn.toInt, sh.toInt)
  }
  def split = rep1sep(terminal, """,""".r) ^^ (new Split(_))
  def chain = rep1sep(split, """\*""".r) ^^ (new Chain(_))

  def argLabel:Parser[ArgLabel] = arga | argm | argNum | rel

  def rel = "rel" ^^^ (Rel)
  def arga = "ARGA" ^^^ (ARGA) //ARGA _never_ has feature
  def argm = "ARGM" ~> "-" ~> feature ^^ {feat => ARGM(feat)} //ARGM _always_ has feature
  def argNum =  ("ARG" ~> decimalNumber ~ ("-" ~> feature).?) ^^ { //ARG<N> _might_ have feature.
    case num ~ None => ARGNUM(num.toInt, None)
    case num ~ Some(feat) => ARGNUM(num.toInt, Some(feat))
  }

  def feature = List(Extent, Direction, Location, Temporal, Reciprocal, Predication, Negation, Modal, Adverbial,
    Manner, Cause, PurposeNotCause, Discourse).map (f => (f.feature ^^ (_ => f))).reduce (_ | _) | word ^^ (Preposition(_))

}
