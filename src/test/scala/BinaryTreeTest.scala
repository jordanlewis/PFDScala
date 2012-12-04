package org.jordanlewis.pfds

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import org.scalacheck.Gen._

class BinaryTreeTest extends Spec with ShouldMatchers with Checkers {
  describe("A complete tree") {
    it("should have 2^n - 1 nodes") {
      check(forAll(choose(0, 25)){n: Int =>
          BinaryTree.complete(0, n).size == scala.math.pow(2, n) - 1 })
    }
  }

  describe("A balanced tree") {
    it("should have n nodes") {
      check(forAll(choose(0, 100)){n: Int =>
          BinaryTree.balanced(0, n).size == n})
    }
  }
}
