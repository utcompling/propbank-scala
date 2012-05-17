package srl.features
import edu.stanford.nlp.trees._
import edu.stanford.nlp.ling._
import srl.propbank._
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

sealed abstract class SequenceItem(val label:String) {
  def featureString(idx:Int):String
  override def toString = label + featureString(0)
  def appendFeature(sb:StringBuilder, name:String, index:Int, value:String):StringBuilder = {
    sb append "\t" append name append "[" append index.toString append "]" append "=" append value
  }
}

case object Start extends SequenceItem("<start />") {
  def featureString(idx:Int):String = ""
}

case object End extends SequenceItem("<end />") {
  def featureString(idx:Int):String = ""
}

case class UnlabelledItem(token:String, pos:String) extends SequenceItem("NONE") {
  def featureString(idx:Int) = {
    val sb = appendFeature(new StringBuilder, "token", idx, token)
    appendFeature(sb, "pos", idx, pos) mkString
  }
}

case class LabelledItem(
  override val label:String, //this is what's being learned.
  token:String, //this is the word itself.
  pos:String, //part of speech isn't too bad...
  verb:String, //do we want to give it the verb? //sure: assume a chunker would figure it out.
  inflection:Inflection
) extends SequenceItem(label){
  val features = List(
    ("token", token),
    ("pos", pos),
    ("verb", verb))

  /**
   * produce a feature string with the index based at idx.
   */
  def featureString(idx:Int) ={
    val base = features.foldLeft(new StringBuilder) {
      (sb, feat) => appendFeature(sb, feat._1, idx, feat._2)
    } 
    val inflected = addInflection(base, idx)
    inflected mkString
  }

  /**
   * Add inflection features.
   */
  def addInflection(strbl:StringBuilder, idx:Int):StringBuilder = {
    val names = List("form", "tense", "aspect", "person", "voice")
    val comps = inflection.l map (_.comp) 
    (names zip comps).foldLeft(strbl) {
      (sb, pair) => pair match {
        case (_, "-") => sb
        case (n, c) => appendFeature(sb, n, idx, c)
      }
    }
  }
}

/**
 * Extracts a labelled sequence from a tree.
 */
class SequenceExtractor(tree:Tree) extends ArgsExtractor(tree) {
  val reversedTerms = terminals.indices.map {
    idx => terminals(idx) -> idx
  }.toMap[Tree, Int]

  /**
   * Produce a list of all the terminals that an argument covers.
   * @param arg An ArgInfo object.
   * @return A single list of terminals as trees.
   */
   def coveredTerms(arg:ArgInfo):List[Tree] =  arg.content.flatten.flatMap { 
     tree => (new TreeExtractor(tree)).terminals.toList
   }

  /**
   * Produce an array of SequenceItems.
   * @param instances The list of propbank instances to extract from
   * @return An array of SequenceItems representing the sequence, in order.
   */
  def extractSequence(instances:List[LineRep]):Array[SequenceItem] = {
    val args = extractArgInfo(instances)
    val items = args.flatMap {
      arg => coveredTerms(arg) map (_ -> arg)
    }.toMap[Tree, ArgInfo] transform {
      (t, arg) => LabelledItem(arg.arg,
        t.label.value,
        t.ancestor(1, tree).label.value,
        arg.verb, arg.verbInfl)
    }
    terminals map {
      term => items getOrElse(term, UnlabelledItem(term.label.value,
        term.ancestor(1, tree).label.value))
    }
  }
}

/**
 * Class allowing access to sequence elements.
 * @param ls An array of sequence items.
 */
class LabelledSequence(val ls:Array[SequenceItem]) {
  private def surround(window:Int) = -window until window withFilter(_ != 0)
  /**
   * Get sequence items, with bounds-checking.
   * @param idx An index into the sequence.
   * @return A SequenceItem, either one from the array, or the Start or End object.
   */
  def get(idx:Int) = if (idx < 0) Start else if (idx >= ls.length) End else ls(idx)

  /**
   * Get a labelled element, along with a window around it.
   * @param idx Which element in the sequence to get.
   * @param window How many items before and after to bring in.
   * @return The item at idx, followed by the (window) elements before and (window) elements after.
   * @see get, above
   */
  def getWindow(idx:Int, window:Int) = get(idx)::(surround(window).map {
      s => get(idx + s)
    } toList)

  /**
   * Create a string for an index with a window.
   * @param idx Which element in the sequence to start with.
   * @param window How many items before and after to bring in.
   * @return A feature string
   * @see getWindow, above
   */
   def mkFeatureString(idx:Int, window:Int):String = {
     val sb = (new StringBuilder) append get(idx).toString
     surround(window)
     .map { s => get(idx + s).featureString(s) }
     .foldLeft(sb) (_ append  _)
     .mkString
   }
   /**
    * Produce a Windowed object with a size over this LabelledSequence.
    */
   def window(window:Int) = new Windowed(window, this)

   class Windowed(val window:Int, val lseq:LabelledSequence) { 
     def get(idx:Int) = lseq.getWindow(idx, window)

     def foreach(f: String => Unit):Unit = lseq.ls.indices foreach {
       i => f(mkFeatureString(i, window))
     }
   }
}
