package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton CodeTree") {
    val oneLeaf = List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3))
    assert(singleton(oneLeaf) === true)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of another leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('c', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('c', 2), Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of simple leaf list") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === List(Leaf('e', 1)))
  }
  
  test("until of some list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x', 4), List('e', 't', 'x'), 7)))
  }
  
   test("decode some bits") {
    val codeTree = Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x', 4), List('e', 't', 'x'), 7)
    val bi = List(0,0,1,0,1)
    assert(decode(codeTree, bi) === List('e', 'x', 't'))
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("convert to some CodeTable") {
    val ct = Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x', 4), List('e', 't', 'x'), 7)
    assert(convert(ct) === List(('e',List(0, 0)), ('t',List(0, 1)), ('x',List(1))))
  }

  test("bit sequence of char") {
    val ct = List(('e',List(0, 0)), ('t',List(0, 1)), ('x',List(1)))
    assert(codeBits(ct)('t') === List(0, 1))
  }
  
}
