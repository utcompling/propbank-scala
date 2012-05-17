package srl.propbank
import scalaz._
import Scalaz._ //for the intersperse call

//Everything needed to represent a line in PropBank
//take a look at http://www.ldc.upenn.edu/Catalog/docs/LDC2004T14/readme.txt

/**
 * Allows the mixing-in of stringWithSep
 */
trait ListStringer {
  /**
   * Turns a list into a string with a seperator.
   * @tparam A The type of elements in the list.
   * @param l A list to be turned into a string.
   * @param sep The seperator to use on items in the list.
   * @return A string, containing the string reps of each list item, with sep in between.
   */
  def stringWithSep[A](l:List[A], sep:String):String = l.map(_ toString)
    .intersperse(sep)
    .foldLeft(new StringBuilder)(_ ++= _)
    .mkString
}

class LineRep (
  val wsjFile:String, //name of the wsj file
  val sentence:Int, //index of the sentence in the wsj file, from 0
  val terminal:Int, //index, from 0, of the verb terminal
  val tagger:String, //which annotator, "gold" for multiple/adjucated
  val frameSet:FrameSet, //references the frameset in the verb's framefile
  val inflection:Inflection,  //verb's inflection
  val labels:List[Label]) extends ListStringer {


  override def toString = stringWithSep(List(
    wsjFile, sentence.toString, terminal.toString,
    tagger, frameSet, inflection.toString) ++ labels, " ")
}

//frameset
case class FrameSet(val verb:String, val index:Int) {
  override def toString = verb + ".%02d".format(index)
}

/**
 * Represents the inflection properties of the verb.
 */
case class Inflection(form:Form, tense:Tense, aspect:Aspect, person:Person, voice:Voice) extends ListStringer {
  val l:List[InflComp] = List(form, tense, aspect, person, voice)
  override def toString = stringWithSep(l, "")
}

abstract class InflComp(val comp:String) {
  override def toString = comp
}

abstract class Form(form:String) extends InflComp(form)
case object Infinitive extends Form("i")
case object Gerund extends Form("g")
case object Participle extends Form("p")
case object Finite extends Form("v")
case object NoForm extends Form("-")

abstract class Tense(tense:String) extends InflComp(tense)
case object Future extends Tense("f")
case object Past extends Tense("p")
case object Present extends Tense("n")
case object NoTense extends Tense("-")

abstract class Aspect(aspect:String) extends InflComp(aspect)
case object Perfect extends Aspect("p")
case object Progressive extends Aspect("o")
case object Both extends Aspect("b")
case object NoAspect extends Aspect("-")

abstract class Person(person:String) extends InflComp(person)
case object Sing3rd extends Person("3")
case object NoPerson extends Person("-")

abstract class Voice(voice:String) extends InflComp(voice)
case object Active extends Voice("a")
case object Passive extends Voice("p")
case object NoVoice extends Voice("-")

/**
 * Represents a full arguments specification.
 */
case class Label(relation:Relation, argLabel:ArgLabel) {
  override def toString = relation.toString + "-" + argLabel.toString

}

//--relations--
sealed abstract class Relation
class Terminal(val termNum:Int, val height:Int) extends Relation {
  override def toString = termNum.toString + ":" + height.toString
}
object Terminal {
  def apply(termNum:Int, height:Int) = new Terminal(termNum, height)
  def unapply(terminal:Terminal) = Some((terminal.termNum, terminal.height))
}
class Split(val term:List[Terminal]) extends Relation with ListStringer { //a list of terminals
  override def toString = stringWithSep(term, ",")
}
object Split {
  def apply(term:Terminal*) = new Split(term.toList)
  def unapplySeq(split:Split) = Some(split.term)
}
class Chain(val split:List[Split]) extends Relation with ListStringer { //a list of splits
  override def toString = stringWithSep(split, "*")
}
object Chain {
  def apply(split:Split*) = new Chain(split.toList)
  def unapplySeq(chain:Chain) = Some(chain.split)
}

//--arglabels--
abstract class ArgLabel(val label:String, val feature:Option[Feature]) {
  override def toString = label + feature.map("-" + _.toString).getOrElse("")
}

//-labels-
case object Rel extends ArgLabel("rel", None)
case object ARGA extends ArgLabel("ARGA", None)
case class ARGM(feature1:Feature) extends ArgLabel("ARGM", Some(feature1))
case class ARGNUM(num:Int, feature1:Option[Feature]) extends ArgLabel("ARG" + num.toString, feature1)

//-features-
abstract class Feature(val feature:String) {
  override def toString = feature
}
case object Extent extends Feature("EXT")
case object Direction extends Feature("DIR")
case object Location extends Feature("LOC")
case object Temporal extends Feature("TMP")
case object Reciprocal extends Feature("REC")
case object Predication extends Feature("PRD")
case object Negation extends Feature("NEG")
case object Modal extends Feature("MOD")
case object Adverbial extends Feature("ADV")
case object Manner extends Feature("MNR")
case object Cause extends Feature("CAU")
case object PurposeNotCause extends Feature("PNC")
case object Discourse extends Feature("DIS")
case class Preposition(prep:String) extends Feature(prep)

