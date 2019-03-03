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

  test("times") {
    new TestTrees {
      val str = List('a', 'a', 'b', 'c', 'a', 'b')
      val result = times(str)
      assert(result.head._1 === 'a')
    }
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

  test("createCodeTree") {
//    val list = "someText".toList
    val list = "aaaaaaaabbbcdefgh".toList
   val codeTree = createCodeTree(list)
    println(codeTree)
  }

  test("singleton") {
    assert(! singleton(List()))
    assert(singleton(List(Leaf('b',3))))
    assert(! singleton(List(Leaf('b',3), Leaf('d',4))))
  }


  test("combine of some leaf list") {
    new TestTrees {
      val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
      val trees = combine(leaflist)
      assert(trees === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
    }
  }

  test("decode") {
    val chars = decode(frenchCode, secret)
    assert(decodedSecret === chars)
  }

  test("encode 2") {
    new TestTrees {
      val encoded = encode(t2)("abababd".toList)
      private val chars: List[Char] = decode(t2,encoded)
      assert(chars === "abababd".toList)
    }
  }

  test("decode and encode big") {
    new TestTrees {
      private val encoded: List[Bit] = encode(frenchCode)("bonjourmademoisellecava".toList)
      assert(decode(frenchCode, encoded) === "bonjourmademoisellecava".toList)
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("convert small") {
    new TestTrees {
      private val table: CodeTable = convert(t2)
      assert(table === List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
    }
  }

  test("quickEncode + encode small") {
    new TestTrees {
      val encoded = quickEncode(t2)("abd".toList)
      val encoded2 = encode(t2)("abd".toList)
      assert(encoded === encoded2)
      private val chars: List[Char] = decode(t2,encoded)
      assert(chars === "abd".toList)
    }
  }

  test("quickEncode and decode small") {
    new TestTrees {
      val encoded = quickEncode(t2)("abababd".toList)
      private val chars: List[Char] = decode(t2,encoded)
      assert(chars === "abababd".toList)
    }
  }

  test("encode and qencode big") {
    new TestTrees {
      private val encoded: List[Bit] = encode(frenchCode)("bonjourmademoisellecava".toList)
      private val qencoded: List[Bit] = quickEncode(frenchCode)("bonjourmademoisellecava".toList)
      assert(encoded === qencoded)
    }
  }

}
