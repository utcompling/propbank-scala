package srl

package object propbank {
  import edu.stanford.nlp.trees._
  type SentenceRef = (String, Int)

  /**
   * A PropBank Instance is a tree, the original location of the tree, and the
   * list of lines that annotate it.
   */
  case class Instance(tree:Tree, ref:SentenceRef, lines:List[LineRep])
  
  /**
   * Groups instances.
   * @param treeMatched A list of parsed lines matched with trees.
   * @return An instance object for every tree in the list.
   */
  def groupInstances(treeMatched:List[(Tree, LineRep)]):List[Instance] = {
    treeMatched.foldLeft(Map.empty[Tree, List[LineRep]]) {
      (m, pair) => m + (pair._1 -> (pair._2::m.getOrElse(pair._1, Nil)))
    }.toIterable.flatMap {
      case (t, Nil) => None //shouldn't happen, but good to be safe.
      case (t, l @ List(head, _*)) => Some(Instance(t, (head.wsjFile -> head.sentence), l))
    }.toList
  }

  /*
  def fileMap(props:Map[Tree, List[LineRep]]):Map[Tree, (String, Int)] = props.transform {
    (tree, instances) => (instances.head.wsjFile, instances.head.sentence)
  }*/

  /**
   * Load up propbank instances.
   * @param propBankPath Path to PropBank's prop.txt
   * @param treeBankPath Path to directory above wsj/ in penn-treebank.
   * @return A stream over instances.
   */
  def loadInstances(propBankPath:String, treeBankPath:String):Stream[Instance]= {
    val lines = InstanceLoader.load(propBankPath)
    val treeLoader = new TreeLoader(treeBankPath)
    treeLoader.loadAll(lines) flatMap {
      l => groupInstances(l) 
    }
  }
}
