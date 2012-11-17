package com.rschonberger.algorithm

import collection.immutable.{HashSet, HashMap}
import annotation.tailrec
import collection.mutable
import java.util.NoSuchElementException

object StableMarriage {
  def finalMarriages[A, B, C <% Ordered[C]](men: Seq[A], women: Seq[B], scoreFunction: (A, B) => C): Map[B, A] = {
    val marriage = new StableMarriage(men, women, scoreFunction)
    marriage.finalMarriages
  }
}

class StableMarriage[A, B, C <% Ordered[C]] protected(men: Seq[A], women: Seq[B], val scoreFunction: (A, B) => C) {
  type ScoreType = (C, B)

  private implicit object TupleOrdering extends Ordering[ScoreType] {
    def compare(a: ScoreType, b: ScoreType) = a._1 compare b._1
  }

  lazy val proposalCache: Map[A, mutable.PriorityQueue[ScoreType]] = men map (man => man -> new mutable.PriorityQueue[ScoreType]) toMap
  
  lazy val finalMarriages: Map[B, A] = {
    val freeMen: Seq[A] = men
    val currentMarriages: Map[B, A] = HashMap.empty
    // TODO(Robsc): performence test these two different algorithms.
    if (men.size < 100) {
      findMarriages(freeMen, currentMarriages)
    } else {
      findMarriages(freeMen, HashSet.empty, currentMarriages)
    }
  }

  def getProposalQueue(man: A): mutable.PriorityQueue[ScoreType] = {
    proposalCache.get(man) match {
      case None => throw new NoSuchElementException("Unknown man")
      case Some(queue) if queue.nonEmpty => queue
      case Some(queue) => {
        queue ++= women map (woman => (scoreFunction(man, woman), woman))
      }
    }
  }

  @tailrec final def findMarriages(men: Seq[A], currentMarriages: Map[B, A]): Map[B, A] = {
    men match {
      case freeMan :: tailMen => {
        val womanQueue = getProposalQueue(freeMan)

        val (newScore, woman) = womanQueue.dequeue

        val currentChosenMan = currentMarriages.get(woman)
        lazy val marriages: Map[B, A] = currentMarriages + (woman -> freeMan)
        currentChosenMan match {
          case None => {
            findMarriages(tailMen, marriages)
          }
          case Some(man) => {
            if (scoreFunction(man, woman) < newScore) {
              // Choose a new man instead of the old one
              findMarriages(man +: tailMen, marriages)
            } else {
              findMarriages(men, currentMarriages)
            }
          }         
        }
      }
      case _ => currentMarriages
    }
  }

  @tailrec final def findMarriages(men: Seq[A], previousProposals: Set[(A, B)], currentMarriages: Map[B, A]): Map[B, A] = {
    men match {
      case freeMan :: tailMen => {
        // Pick a woman to propose to
        val womanQueue = getProposalQueue(freeMan)

        val woman: B = women filter (!previousProposals.contains(freeMan, _)) maxBy (scoreFunction(freeMan, _))
                
        // We are definitely proposing.
        // val woman = chosenWoman
        val newScore = scoreFunction(freeMan, woman)
        val newProposal = (freeMan -> woman)
        val proposals: Set[(A, B)] = previousProposals + newProposal
        val currentChosenMan = currentMarriages.get(woman)
        lazy val marriages: Map[B, A] = currentMarriages + (woman -> freeMan)
        currentChosenMan match {
          case None => {
            findMarriages(tailMen, proposals, marriages)
          }
          case Some(man) => {
            val currentScore = scoreFunction(man, woman)
            if (currentScore < newScore) {
              // Choose a new man
              findMarriages(man +: tailMen, proposals, marriages)
            } else {
              findMarriages(men, proposals, currentMarriages)
            }
          }
        }
      }
      case _ => currentMarriages
    }
  }
}