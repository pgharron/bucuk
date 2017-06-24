package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("find min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == scala.math.min(a, b)
  }

  property("ins and delete") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("delete min") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(deleteMin(h))) == scala.math.max(c, scala.math.max(a, b))
  }

  property("sorting a heap") = forAll { h: H =>
    def isSorted(min: Int, h: H): Boolean =
      if (isEmpty(h))
        true
      else if(min > findMin(h))
        false
      else
        isSorted(findMin(h), deleteMin(h))

    isSorted(findMin(h), deleteMin(h))
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)

    val min = findMin(m)

    min == findMin(h1) || min == findMin(h2)
  }


}
