package patmat

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    val values = times(string2Chars("hello, world"))
    assert(values.contains(('l', 3)))
    assert(values.contains(('o', 2)))
    assert(values.contains(('w', 1)))
    assert(values.contains(('r', 1)))
    assert(values.contains(('h', 1)))
    assert(values.contains(('e', 1)))
    assert(values.contains(('d', 1)))
    assert(values.contains((',', 1)))
    assert(values.contains((' ', 1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton of 2") {
    assert(singleton(List(Leaf('e', 1), Leaf('t', 2))) == false)
  }

  test("singleton") {
    assert(singleton(List(Leaf('e', 1))) == true)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("createCodeTree") {
  }

  test("createCodeTree empty") {
    try {
      createCodeTree(string2Chars(""))
      fail()
    } catch {
      case ex: IllegalArgumentException => assert(true)
    }
  }

  test("decode") {
    new TestTrees {
      assert(decode(t1, List(0,1,0,0,0,1,0)) === "abaaaba".toList)
    }
  }

  test("decodeSecret") {
    new TestTrees {
      println(decodedSecret)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a other text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("abaaaba".toList)) === "abaaaba".toList)
    }
  }

  test("decode and encode a other text should be identity with CodeTable") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("abaaaba".toList)) === "abaaaba".toList)
    }
  }

}
