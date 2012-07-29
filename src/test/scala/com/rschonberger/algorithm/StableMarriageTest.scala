package com.rschonberger.algorithm

import org.scalatest._
import collection.immutable.HashSet

class StableMarriageSuite extends FunSuite {
  test("The x is cheese") {
    expect("Cheese")(StableMarriage.x)
  }

  test("One Person example") {
    val men: Seq[String] =  "a" :: List.empty
    val women: Set[Int] = HashSet.empty + 1
    val expectedResult = Map.empty + (1 -> "a")
    val score = (a: String, b: Int) => 1
    val actualResult = StableMarriage.finalMarriages(men, women, score)
    expect(expectedResult)(actualResult)
  }
}