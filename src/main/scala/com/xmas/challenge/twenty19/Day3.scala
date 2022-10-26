package com.xmas.challenge.twenty19

import scala.annotation.tailrec
import scala.io.Source

object Day3 {
  /**
   * Find the intersection point closest to the central port
   */

  lazy val wires: List[List[String]] = Source.fromResource("Day3Input.txt").getLines.toList.map(_.split(',').toList)
  lazy val defaultWires: (List[String],List[String]) = (wires.head,wires.last)
  lazy val wireA: List[String] = defaultWires._1
  lazy val wireB: List[String] = defaultWires._2

  object Point {
    def zero:Point = Point(0,0,0)
  }

  case class Point(x: Int , y: Int, steps: Int)

  object Segment {
    def empty: Segment = Segment(Point.zero, Point.zero)
  }

  case class Segment(start: Point, end: Point)


  private def calculateNewCoordinate(rule: String, point: Point): Point = rule.head match {
    case 'U' ⇒ point.copy(y = point.y + rule.drop(1).toInt, steps = point.steps + rule.drop(1).toInt)
    case 'D' ⇒ point.copy(y = point.y - rule.drop(1).toInt, steps = point.steps + rule.drop(1).toInt)
    case 'R' ⇒ point.copy(x = point.x + rule.drop(1).toInt, steps = point.steps + rule.drop(1).toInt)
    case 'L' ⇒ point.copy(x = point.x - rule.drop(1).toInt, steps = point.steps + rule.drop(1).toInt)
    case _ ⇒ throw new Exception("Unknown rule")
  }

  def createSegments(wire: List[String]): List[Segment] =
  wire
    .foldLeft( List(Point.zero) )( (points, rule) ⇒ points :+ calculateNewCoordinate(rule, points.last) )
    .foldLeft( List(Segment.empty) )((acc, p) ⇒ acc :+ Segment(acc.last.end, p) )
    .drop(2)


  private def segRange(a: Int, b: Int): List[Int] = a match {
    case min if min <= b ⇒ (a to b).toList
    case _ ⇒ (b to a).toList
  }

  private def getSegmentRange(s: Segment): List[Point] = {
    if (s.start.x == s.end.x) segRange(s.start.y, s.end.y).map(r ⇒ Point(s.start.x, r, s.start.steps + (s.start.y - r).abs ))
    else segRange(s.start.x, s.end.x).map( r ⇒ Point(r, s.start.y, s.start.steps + (s.start.x - r).abs))
  }


  private def compareSegments(seg1: Segment, seg2:Segment): Boolean = {
    !(seg1.start == Point.zero || seg2.start == Point.zero) &&
      ( ( segRange(seg2.start.x, seg2.end.x).contains(seg1.start.x) && segRange(seg1.start.y, seg1.end.y).contains(seg2.start.y) ) ||
        (segRange(seg2.start.y, seg2.end.y).contains(seg1.start.y) && segRange(seg1.start.x, seg1.end.x).contains(seg2.start.x) ) )
  }

  private def alternateIntersection(wireA: List[String], wireB: List[String]): List[Point] = {
    val crossingPoints = for {
      s1 ← createSegments(wireA)
      s2 ← createSegments(wireB)
    } yield {
      if (compareSegments(s1,s2)) {
        for {
          p1 ← getSegmentRange(s1)
          p2 ← getSegmentRange(s2)
        } yield {
          p1 match {
            case Point(0,0,_) ⇒ None
            case p if p.x == p2.x && p.y == p2.y ⇒ Some(p.copy(steps = p.steps + p2.steps))
            case _ ⇒ None
          }
        }
      }
      else Nil
    }
    crossingPoints.flatten.filter(_.isDefined).map(_.get)
  }

   def calculateShortestDistance(wireA: List[String], wireB: List[String]): Int =
     alternateIntersection(wireA, wireB) match {
      case Nil ⇒ throw new Exception("Wires do not cross")
      case p :: Nil ⇒ p.x.abs + p.y.abs
      case multiple ⇒ min(multiple.map(p ⇒ p.x.abs + p.y.abs))
    }

  def calculateFewestSteps(wireA: List[String], wireB: List[String]): Int =
    alternateIntersection(wireA, wireB) match {
      case Nil ⇒ throw new Exception("Wires do not cross")
      case p :: Nil ⇒ p.steps
      case multiple ⇒ min(multiple.map(p ⇒ p.steps))
    }



  @tailrec
  private def min(numbers: List[Int]): Int = numbers match {
      case Nil ⇒ throw new Exception("No min in an empty list")
      case d :: Nil ⇒ d
      case a :: b :: tail ⇒ min( (if (a < b) a else b) :: tail)
    }

  // Ick
  private def minIterative(numbers: List[Int]): Int = {
    var min: Option[Int] = None
        for {
          i ← 0 to numbers.length
        } yield {
          if(numbers(i) < min.getOrElse(numbers(i))) { min = Some(numbers(i))}
        }
      min.get
  }
















  //  def createSegments(wire: List[String]): List[Segment] =
  //    wire
  //      .reverse
  //      .foldRight( List(Point.zero) )( (rule, points) ⇒ calculateNewCoordinate(rule, points.head) +: points )
  //      .foldRight( List(Segment.empty) )((p,acc) ⇒ Segment(acc.head.end, p) +: acc)
  //      .reverse
  //      .drop(2)

//  def createPoints2(wire: List[String], acc: List[Point] = List(Point(0,0))): List[Point] = wire match {
//
//    case Nil ⇒ acc
//    case rule :: Nil ⇒ acc :+ calculateNewCoordinate(rule, acc.last)
//    case rule :: tail ⇒ createPoints2(tail, acc :+ calculateNewCoordinate(rule, acc.last))
//  }

  //  private def getSegmentRange(s: Segment): List[Point] =
  //    s match {
  //      case horz if horz.start.x == horz.end.x ⇒ (s.start.y to s.end.y).map(Point(s.start.x, _)).toList
  //      case vert if vert.start.y == vert.end.y ⇒ (s.start.x to s.end.x).map(Point(_, s.start.y)).toList
  //    }

  //Brute Force? - for each segment of wire A, is there a segment on wire B that crosses it?
  //slope = y2-y1 / x2 - x1
//  def findCrossingPoints(wireA: List[String], wireB: List[String]): List[Point] = {
//    val crossingPoints: List[Option[Point]] = for {
//      s1 ← createSegments(wireA)
//      p1 ← getSegmentRange(s1)
//      s2 ← createSegments(wireB)
//      p2 ← getSegmentRange(s2)
//    } yield {
//      println(s"Checking $p1 & $p2")
//      p1 match {
//        case Point(0,0) ⇒ None
//        case p if p == p2 ⇒ Some(p)
//        case _ ⇒ None
//      }
//    }
//    crossingPoints.filter(_.isDefined).map(_.get)
//  }
//
//  private def compareSegments2(seg1: Segment, seg2:Segment): Boolean = {
//    if(seg1.start == Point.zero || seg2.start == Point.zero) false
//    if( segRange(seg2.start.x, seg2.end.x).contains(seg1.start.x) && segRange(seg1.start.y, seg1.end.y).contains(seg2.start.y) ) true
//    else if (segRange(seg2.start.y, seg2.end.y).contains(seg1.start.y) && segRange(seg1.start.x, seg1.end.x).contains(seg2.start.x) ) true
//    else false
//  }

}
