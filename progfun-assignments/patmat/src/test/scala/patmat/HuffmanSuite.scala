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
    assert(times(string2Chars("hello, world")) === List(('l', 3), ('o', 2), ('w', 1), ('r', 1), ('h', 1), ('e', 1), ('d', 1), (',', 1), (' ', 1)))
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
    assert(createCodeTree(string2Chars("hola")) === Fork(Fork(Fork(Leaf('o', 1), Leaf('l', 1), List('o', 'l'), 2), Leaf('h', 1), List('o', 'l', 'h'), 3), Leaf('a', 1), List('o', 'l', 'h', 'a'), 4))
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
