package patmat

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match { // tree match ...
    case Fork(l, r, c, weight) => weight
    case Leaf(c, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match { // tree match ...
    case Fork(l, r, chars, w) => chars
    case Leaf(char, w) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def addChar(inputChar: Char, pList: List[(Char, Int)]): List[(Char, Int)] = pList match {
    // if the list is empty, then this character has not been encountered before. Add a new tuple of (inputChar, 1)
    case List() => List((inputChar, 1))

    // if the list is not empty and ...
    //   if the inputChar matches the head, add 1 to the counter and append the rest of the data
    //  otherwise, leave the head as-is and add the inputChar to the remaining pairs
    case (c: Char, i: Int) :: tailPairs => {
      if (c == inputChar) { (c, i + 1) :: tailPairs }
      else { (c, i) :: addChar(inputChar, tailPairs) }
    }
  }

  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case List() => List()
    case x :: xs => addChar(x, times(xs))
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def insertLeaf(leaf: Leaf, existingList: List[Leaf]): List[Leaf] = existingList match {
    case List() => List(leaf)
    case x :: xs => {
      if (leaf.weight > x.weight) x :: insertLeaf(leaf, xs)
      else leaf :: existingList
    }
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
    case List() => List()
    case x :: xs => insertLeaf(Leaf(x._1, x._2), makeOrderedLeafList(xs))
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case List(x) => true
    case _ => false
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case node1 :: node2 :: treeTail => {
      makeCodeTree(node1, node2) :: treeTail
    }
    case _ => trees
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (done(trees)) trees
    else merge(trees) ::: until(done, merge)(trees.tail)
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val freqs: List[(Char, Int)] = times(chars)
    val treeList: List[Leaf] = makeOrderedLeafList(freqs)

    val combinedNodes = until(singleton, combine)(treeList)

    combinedNodes.head
  }


  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def getOneChar(tree: CodeTree, bits: List[Bit]): (Char, Int) = tree match {
    // helper function to retrieve one character and keep track of the
    // maximum tree depth reached during search
    case Leaf (c, i) => (c, 0)
    case Fork (l, r, c, w) => {

      val (foundChar, depth) = bits match {
        case List() => throw new Exception("Length or bits incompatible with tree structure ")
        case 0 :: tail => getOneChar(l, tail)
        case 1 :: tail => getOneChar(r, tail)
      }
      (foundChar, depth + 1)

    }
  }

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = bits match {
    case List() => List()
    case _ => {
      val (foundChar:Char, foundAtDepth:Int) = getOneChar(tree, bits)

      // return the plumbed character and decode the remaining bits
      foundChar :: decode(tree, bits.splitAt(foundAtDepth)._2)
    }

  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def getBitSequence(tree:CodeTree, singleChar: Char): List[Bit] = tree match {
      case Leaf(c, i) => List()
      case Fork(l, r, c, w) => {
        if (chars(l).contains(singleChar)) 0 :: encode(l)(List(singleChar))
        else if (chars(r).contains(singleChar)) 1 :: encode(r)(List(singleChar))
        else throw new Exception("Character not found in tree")
      }
    }

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = text match {
    case List() => List()
    case x :: xs => getBitSequence(tree, x) ::: encode(tree)(xs)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case List() => throw new Exception("char not found in table")
    case (headChar, bitList) :: xs => {
      if (headChar == char) bitList
      else codeBits(xs)(char)
    }
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */

  def convert(tree: CodeTree): CodeTable = tree match {
    case Leaf(c, w) => List((c, List[Bit]()))
    case Fork(l, r, c, w) => mergeCodeTables(convert(l), convert(r))
  }
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    def prepend(valToPrepend:Bit, codeTableElement:(Char, List[Bit])): (Char, List[Bit]) = {
      // helper function to prepend a bit to the map element's bit list
      (codeTableElement._1, valToPrepend :: codeTableElement._2)
    }
    a.map(x => prepend(0, x)) :::
    b.map(x => prepend(1, x))
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table:CodeTable = convert(tree)
    text.flatMap(codeBits(table))
  }
}

object Huffman extends Huffman
