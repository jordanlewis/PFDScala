package org.jordanlewis.PFDScala

import org.scalatest.matchers.ShouldMatchers
import org.jordanlewis.pfds.UnbalancedTreeSet
import org.scalatest.Spec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class UnbalancedSetTest extends Spec with ShouldMatchers {
  describe("An unbalanced set") {
    it("should contain elements that have been inserted") {
      val set = UnbalancedTreeSet[Int].insert(3).insert(5).insert(2)
      set.member(3) should be (true)
      set.member(2) should be (true)
      set.member(5) should be (true)
      set.member(7) should be (false)
    }
  }
}