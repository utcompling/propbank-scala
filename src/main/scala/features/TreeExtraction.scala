package srl.features
import edu.stanford.nlp.trees._
import edu.stanford.nlp.ling._
import srl.propbank._
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

/**
 * A class for extracting relations from a tree.
 * @param tree Tree to extract from.
 */
class TreeExtractor(val tree:Tree) {
  //a bit ugly, but it does need to be an array.
  val terminals:Array[Tree] = (ArrayBuffer.empty ++ tree.getLeaves).toArray

  def extractRel(terminal:Terminal) = tree.ancestor(terminal.height, terminals(terminal.termNum))
  def extractSplit(split:Split) = split.term map (extractRel(_))
  def extractChain(chain:Chain) = chain.split map (extractSplit(_))

  /**
   * Takes the tree that is height above terminal termIdx.
   * @param termIdx The terminal to start search from.
   * @param height How high to go up from the terminal.
   * @return A tree.
   * @note Uses height+1 because propbank calls a (tag word) a terminal,
   * while the Tree class considers word to be the terminal.
   */
  protected def extract1(termIdx:Int, height:Int) = {
    terminals(termIdx).ancestor(height + 1, tree)
  }

  /**
   * Extracts a relation from the tree.
   * @param rel A relation.
   * @return A list representing the chains, containing lists representing the terminal arguments, allowing for splits.
   */
  def extract(rel:Relation):List[List[Tree]] = rel match {
    case Terminal(termIdx, height) => List(List(extract1(termIdx, height)))
    case Split(terms @ _*) => {List((terms flatMap (extract(_).flatten)).toList)}
    case Chain(splits @ _*) => { (splits flatMap extract).toList}
  }

  /**
   * @param A terminal index.
   * @return The string of the label contained in the terminal.
   */
  def term(term:Int):String = terminals(term).label.value
}

/**
 * Represents an argument, with the params and all.
 */
case class ArgInfo(val arg:String, val verb:String, val verbInfl:Inflection, 
  val feature:Option[String], val content:List[List[Tree]])

/**
 * An extractor that extracts ArgInfo objects for the instance list.
 */
class ArgsExtractor(tree:Tree) extends TreeExtractor(tree) {
  /**
   * Extract an ArgInfo object for every argument in the instance list.
   */
  def extractArgInfo(instances:List[LineRep]):List[ArgInfo] = instances flatMap {
    i => i.labels map {
      l => ArgInfo(l.argLabel.label, term(i.terminal), 
        i.inflection, l.argLabel.feature map (_ feature), 
        extract(l.relation))
    }
  }
}
