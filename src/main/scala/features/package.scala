package srl

package object features {
  //implicit def arraySeqItem2LabelledSequence(arrSeq:Array[SequenceItem]):LabelledSequence = new LabelledSequence(arrSeq)
  import edu.stanford.nlp.trees.Tree
  import srl.propbank.{LineRep, Instance}

  /**
   * Extract a labelled sequence from an Instance object.
   */
  def extractSequence(instance:Instance):LabelledSequence = instance match {
    case Instance(tree, ref, lines) => new LabelledSequence(
      (new SequenceExtractor(tree)).extractSequence(lines))
  }
}
