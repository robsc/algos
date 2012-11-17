package com.rschonberger.algorithm

import org.scalatest._
import collection.immutable.HashSet

class StableMarriageSuite extends FunSuite {
  test("Two Person Example") {
    val men: Seq[String] = Seq("a", "b")
    val women: Seq[Int] = Seq(1, 2)
    val expectedResult = Map(1 -> "b", 2 -> "a")
    val score = (a: String, b: Int) => {
      (a, b) match {
        case ("a", 2) => 2
        case ("b", 1) => 2
        case _ => 1
      }
    }
    val actualResult = StableMarriage.finalMarriages(men, women, score)
    expect(expectedResult)(actualResult)
  }


  test("One Person example") {
    val men: Seq[String] =  Seq("a")
    val women: Seq[Int] = Seq(1)
    val expectedResult = Map(1 -> "a")
    val score = (a: String, b: Int) => 1
    val actualResult = StableMarriage.finalMarriages(men, women, score)
    expect(expectedResult)(actualResult)
  }
}