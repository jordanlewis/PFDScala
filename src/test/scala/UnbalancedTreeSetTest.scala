package org.jordanlewis.pfds

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.scalatest.junit.JUnitRunner

class UnbalancedTreeSetTest extends Spec with ShouldMatchers {
  describe("An unbalanced set") {
    it("should contain elements that have been inserted") {
      val set = new UnbalancedTreeSet[Int].insert(3).insert(5).insert(2)
      set.member(3) should be (true)
      set.member(2) should be (true)
      set.member(5) should be (true)
      set.member(7) should be (false)
    }
  }
}
