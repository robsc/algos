package com.rschonberger.algorithm

import org.scalatest._

class StableMarriageSuite extends FunSuite {
  test("The x is cheese") {
    val myCheese: String = "Cheese"
    assert (myCheese == StableMarriage.x)
  }
}