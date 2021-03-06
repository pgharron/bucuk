package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    *  - test
    *  - ignore
    *  - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {

    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s7 = singletonSet(7)
    val s1000 = singletonSet(1000)
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s2, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      //      println("***** " + FunSets.toString(s))
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect returns a set containing elements in both sets") {
    new TestSets {
      val s = intersect(s1, s1)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff set of elements in p1 but not in p2") {
    new TestSets {
      val s = diff(s1, s3)
      assert(contains(s, 1), "diff 1")
      assert(!contains(s, 2), "diff 2")
      assert(!contains(s, 3), "diff 3")
    }
  }

  test("filter with predicate") {
    new TestSets {
      val s = filter(s2, _ > 1)
      //      printSet(s)
      assert(!contains(s, 1), "filter 1")
      assert(contains(s, 2), "filter 2")
      assert(contains(filter(s2, _ == 2), 2), "! filter 2")
    }
  }

  test("exists given {1,2,3,4} should contain 2") {
    new TestSets {
      val s = union(union(s1, s2), union(s3, s4))
      assert(exists(s, x => x == 2), "Should contain 2")
      assert(!exists(s, _ > 5), "> 5 not exists")
    }
  }

  test("for all") {
    new TestSets {
      val s = singletonSet(1)
      assert(forall(s, x => x >= -1000))
    }
  }

  test("for all {1,3,4,5,7,1000}") {
    new TestSets {
      val s = union(union(union(s1, s3), union(s4, s5)), union(s7, s1000))
      assert(!forall(s, x => x < 5))
    }
  }

  test("map {1,3,4,5,7,1000}") {
    new TestSets {
      assert(contains(map(s1, x => x + 1), 2))

//      for (i <- List(1,3,4,5,7,1000)) {
//        val s = singletonSet(i)
//        assert(contains(map(s, x => x - 1), i - 1))
//      }
    }
  }


}
