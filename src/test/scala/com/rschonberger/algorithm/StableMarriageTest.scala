package com.rschonberger.algorithm

import org.scalatest._

class StableMarriageSuite extends FunSuite {
  test("The x is cheese") {
    expect("Cheese")(StableMarriage.x)
  }
}