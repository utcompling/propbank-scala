import org.specs2._
import srl.propbank._

class ParserSpec extends Specification with matcher.ParserMatchers {
  def is ="Testing a parser"  ^
  p^
  "testing various bits" ^
  "get TMP feature" ! (parsers.feature must succeedOn("TMP").withResult(Temporal)) ^
  "get MOD feature" ! (parsers.feature must succeedOn("MOD").withResult(Modal)) ^
  "get an ARGA label" ! (parsers.arga must succeedOn("ARGA").withResult(ARGA)) ^
  "get the ARGM label" ! (parsers.argm must succeedOn("ARGM-MOD").withResult(ARGM(Modal))) ^
  "get part of a label" ! (parsers.argLabel must succeedOn("ARGM-MOD")) ^
  "get a label" ! (parsers.label must succeedOn("12:0-ARGM-MOD")) ^
  "get a pair of labels" ! (parsers.labels must succeedOn("10:1-ARG0 12:0-ARGM-MOD")) ^
  p^
  "Sample testing..." ^
  "ex0 should parse" ! parseEx(ex0) ^
  "ex1 should parse" ! parseEx(ex1) ^
  "ex2 should parse" ! parseEx(ex2) ^
  "ex3 should parse" ! parseEx(ex3) ^
  "ex4 should parse" ! parseEx(ex4) ^
  "ex5 should parse" ! parseEx(ex5) ^
  "ex6 should parse" ! parseEx(ex6) ^
  end

  val parsers = new LineParserComb

  def parseEx(ex:String) = parsers.lineRep must succeedOn(ex).withResult(equalTo(ex) ^^ { (res:Any) => res.toString})

  val ex0 = "wsj/00/wsj_0003.mrg 18 13 gold regulate.01 ----a 10:1-ARG0 12:0-ARGM-MOD 13:0-rel 14:2-ARG1 20:2-ARGM-MNR"
  val ex1 = "wsj/00/wsj_0003.mrg 1 11 gold enter.01 vn-3a 10:1-ARG0 11:0-rel 12:1-ARG1"
  val ex2 = "wsj/00/wsj_0034.mrg 29 13 gold tie.01 p---p 13:0-rel 9:1*14:0-ARG1 15:1-ARG1-to"
  val ex3 = "wsj/00/wsj_0034.mrg 18 2 gold offer.01 vn--a 0:1-ARG0 2:0-rel 3:2-ARG1"
  val ex4 = "wsj/00/wsj_0034.mrg 11 3 gold account.01 vn--a 0:1-ARG0 2:1-ARGM-TMP 3:0-rel 4:1-ARG1-for"
  val ex5 = "wsj/00/wsj_0032.mrg 1 19 gold expire.01 i---a 0:2*17:0-ARG1 19:0-rel 20:1-ARGM-TMP"
  val ex6 = "wsj/00/wsj_0038.mrg 1 25 gold accrue.01 p---p 25:0-rel 26:0-ARG1"


}
