package srl;
import edu.stanford.nlp.trees._
import java.io.{BufferedWriter, FileWriter, PrintWriter}

/**
 * The main "class" of the srl project.
 */
object SrlTransform {
  val windowSize = 2
  
  /**
   * The manager does the work of loading and outputting.
   * @param propBankPath The path to propbank's prop.txt
   * @param tbPath The path to penn-treebank. On cs machines: /projects/nlp/penn-treebank3/parsed/mrg/
   * @param seqsPath Filepath to output the sequences into.
   * @param filesPath Where to output the sentence references for each sequence to.
   */
  class Mgr(propBankPath:String, tbPath:String, seqsPath:String, filesPath:String) {
    lazy val instanceStream = propbank.loadInstances(propBankPath, tbPath)

    /**
     * Pass it a function that does some writing!
     * @param path A path to open up for writing.
     * @param f A function that takes a PrintWriter and does some work.
     * @note This will close the writer.
     */
    def writeIn(path:String)(f: PrintWriter => Unit):Unit = {
      val writer = new PrintWriter(new BufferedWriter(new FileWriter(path)))
      f(writer)
      writer.close
    }

    /**
     * Get the work done after opening up the sequences and references files.
     */
    def doWrite = writeIn(seqsPath) {
      seqs => writeIn(filesPath) {
        files => doTrees(seqs, files)
      }
    }

    /**
     * Do work by iterating over the instances, transforming them, and outputting them.
     */
    def doTrees(seqs:PrintWriter, files:PrintWriter) = instanceStream foreach {
      instance => {
        instance.ref match {
          case (wsjFile, sentIdx) => files.println(wsjFile + " " +sentIdx)
        }
        features
        .extractSequence(instance)
        .window(windowSize)
        .foreach(seqs.println _)
        seqs.println
      }
    }
  }

  /**
   * Main:
   * (run) <propbank path> <treebank path> <sequences output path> <file refs output path>
   * Creates a manager with those, and tells it to write.
   */
  def main(args:Array[String]) = {
    val mgr = new Mgr(args(0), args(1), args(2), args(3))
    mgr.doWrite
  }
}
