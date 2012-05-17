package srl.propbank
import scala.collection.JavaConversions._
import scala.collection.immutable.Stream
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import edu.stanford.nlp.trees._
import java.io._

/**
 * An object to load instances.
 */
object InstanceLoader {
  /**
   * Loads all the available instances in dataPath.
   * @return A list containing every loaded and parsed instance.
   */
  def load(dataPath:String):List[LineRep] = {
    val buf = ListBuffer.empty[Option[LineRep]]
    val parsers = new LineParserComb
    try { //warning: very imperative style ahead!
      val reader = new BufferedReader(new FileReader(dataPath))
      var line = reader.readLine
      while (line != null) {
        buf += parsers(line)
        line = reader.readLine
      }
    } catch {
      case (ioe:IOException) => { } //todo maybe put an error print here or something.
    }
    buf.flatten.toList
  }
}

/**
 * A class that loads trees from a single file.
 */
class TreeLoader(val basePath:String) {

  //default normalizer for no normalization.
  val normalizer = new TreeNormalizer 

  //tree reader factory using empty normalizer
  val ptrf = new PennTreeReaderFactory(normalizer)

  /**
   * Lazily loads lines up; each item in the stream contains all the trees in a single treebank file.
   * @param lines A list of parsed lines to load up.
   * @return A stream; each element is a list of matched trees and lines.
   */
  def loadAll(lines:List[LineRep]):Stream[List[(Tree, LineRep)]] = if (lines.isEmpty) Stream.empty else {
    val headFile = lines.head.wsjFile
    val spanned = lines span (_.wsjFile == headFile)
    load(headFile, spanned._1) #:: loadAll(spanned._2)
  }


  /**
   * Loads a set of propbank instances in a single file.
   * @param lines A list of propbank instances, all pointing to a single file.
   * @param file A treebank file to load.
   * @return A list of every obtainable instance and tree pair in the file
   */
  def load(path:String, lines:List[LineRep]):List[(Tree, LineRep)] = {
    val bank = loadBank(path)
    lines flatMap { //flatmap gets rid of the options from lift.
      line => (bank lift line.sentence) map (_ -> line)
    }
  }

  class TreeBuffer(val buf:ArrayBuffer[Tree]) extends TreeVisitor {
    def visitTree(t:Tree) = buf ++= t
  }
 
  /**
   * Loads a tree file.
   * @param path A path to a specific file in the treebank.
   * @return An array containing all the trees in the file.
   * @note Uses non-normalized trees so as to match propbank relspecs
   */
  def loadBank(path:String):Array[Tree] = {
    val tb = new DiskTreebank(ptrf)
    tb.loadPath(basePath + "/" + path)
    /*val visitor = new TreeBuffer(ArrayBuffer.empty)
    tb(visitor)
    vaisitor.buf.toArray*/
    (ArrayBuffer.empty ++ tb).toArray
  }

  /**
   * Attempts to find the appropriate Tree for a propbank instance.
   * @param bank An array of trees all from one file.
   * @param line The instance to match - better be in 
   * @return Optionally, the tree specified by the line.
   */
  def matchLine(bank:Array[Tree], line:LineRep):Option[Tree] = {
    bank lift line.sentence
  }
}
