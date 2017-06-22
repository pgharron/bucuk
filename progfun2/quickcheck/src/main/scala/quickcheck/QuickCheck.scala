package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
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


//  lazy val genMap: Gen[Map[Int,Int]] = oneOf(
//    const(Map.empty[Int,Int]),
//    for {
//      k <- arbitrary[Int]
//      v <- arbitrary[Int]
//      m <- oneOf(const(Map.empty[Int,Int]), genMap)
//    } yield m.updated(k, v)
//  )

}
