package com.rschonberger.algorithm

import collection.immutable.{HashSet, HashMap}
import annotation.tailrec
import collection.mutable

object StableMarriage {
  val x: String = "Cheese"

  def finalMarriages[A, B, C <% Ordered[C]](men: Iterable[A], women: Iterable[B], scoreFunction: (A, B) => C): Map[B, A] = {
    val marriage = new StableMarriage(men, women, scoreFunction)
    marriage.finalMarriages
  }
}

class StableMarriage[A, B, C <% Ordered[C]] protected(men: Iterable[A], women: Iterable[B], val scoreFunction: (A, B) => C) {
  type scoreType = (C, B)

  implicit object TupleOrdering extends Ordering[scoreType] {
    def compare(a: scoreType, b: scoreType) = a._1 compare b._1
  }

  val proposalCache: Map[A, mutable.PriorityQueue[scoreType]] = HashMap.empty ++ (men map (man => man -> new mutable.PriorityQueue[scoreType]()))

  lazy val finalMarriages: Map[B, A] = {
    val freeMen = List.empty ++ men
    val freeWomen = HashSet.empty ++ women
    val previousProposals: Set[(A, B)] = HashSet.empty
    val currentMarriages: Map[B, A] = HashMap.empty
    findMarriages(freeMen, freeWomen, previousProposals, currentMarriages)
  }


  @tailrec final def findMarriages(men: Seq[A], women: Set[B], previousProposals: Set[(A, B)], currentMarriages: Map[B, A]): Map[B, A] = {
    men match {
      case Seq(freeMan: A, tailMen@_*) => {
        // Pick a woman to propose to
        var chosenWoman: Option[(B, C)] = None
        for (woman <- women if !previousProposals.contains((freeMan, woman))) {
          val score = scoreFunction(freeMan, woman)
          lazy val newItem = Some(woman, score)
          if (chosenWoman == None) {
            chosenWoman = newItem
          } else {
            val currentScore: Ordered[C] = chosenWoman.get._2
            if (currentScore < score) {
              chosenWoman = newItem
            }
          }
        }
        // We are definitely proposing.
        val (woman, newScore) = chosenWoman.get
        val newProposal = (freeMan, woman)
        val proposals: Set[(A, B)] = previousProposals + newProposal
        val currentChosenMan = currentMarriages.get(woman)
        currentChosenMan match {
          case None => {
            val marriages: Map[B, A] = currentMarriages + (woman -> freeMan)
            findMarriages(tailMen, women, proposals, marriages)
          }
          case Some(man) => {
            val currentScore = scoreFunction(man, woman)
            if (currentScore < newScore) {
              // Choose a new man
              val marriages = currentMarriages + (woman -> freeMan)
              findMarriages(man +: tailMen, women, proposals, marriages)
            } else {
              findMarriages(men, women, proposals, currentMarriages)
            }
          }
          case _ => throw new RuntimeException("What the hell?")
        }
      }
      case _ => currentMarriages
    }
  }
}