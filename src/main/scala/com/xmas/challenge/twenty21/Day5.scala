package com.xmas.challenge.twenty21
import scala.io.Source
/**
 * Day 5 - Advent of Code 2021
 * https://adventofcode.com/2021/day/5
 */
object Day5 {

  private lazy val day5File: String = "Day5_2021_Input.txt"
  lazy val defaultInput: List[String] = Source.fromResource(day5File)
    .getLines.toList
  lazy val defaultLines: List[LineSegment] = getLineSegmentsFromString(defaultInput)

  case class LineSegment(x1:Int, y1:Int, x2: Int, y2: Int)
  case class DiagramPoint(x:Int, y: Int, count: Int = 0)
  type Diagram = Vector[Vector[DiagramPoint]] // using vector for quicker lookups and updates

  //TODO - perhaps have this as a factory method instead of a psuedo-helper object
  object CreateDiagram {
    def apply(size: Int): Diagram = (0 to size).map( y ⇒ {
      (0 to size).map( x ⇒ {
        DiagramPoint(x,y)
      }).toVector
    }).toVector
    //todo quick way to upgrade diagram?
  }

  def countDangerousAreas(lines: List[LineSegment], includeDiagonals: Boolean = false): Int = {
    val max: Int = getMaxXyInSegment.apply(lines.maxBy(getMaxXyInSegment))
    val diagram: Diagram = createDiagramFromLines(lines,includeDiagonals ,max)
    countOverlappingPoints(diagram)
  }

  private val getMaxXyInSegment: LineSegment ⇒ Int = l ⇒ List(l.x1, l.x2, l.y1, l.y2).max

  def countOverlappingPoints(diagram: Diagram, numOverlaps: Int = 2): Int =
    diagram.flatMap(y ⇒ y.filter(point ⇒ point.count >= numOverlaps)).length

  def getLineSegmentsFromString(input: List[String]): List[LineSegment] =
    input.map( str ⇒ {
      val segment = str.split(" -> ").flatMap( point ⇒ point.split(",")).map(_.toInt)
      LineSegment(segment(0), segment(1), segment(2), segment(3))
    })

  private def createDiagonalPoints(ls: LineSegment): List[(Int,Int)] = {
    val xRange = if( ls.x1 < ls.x2 ) ls.x1 to ls.x2 else ls.x2 to ls.x1
    val yRange = if( ls.y1 < ls.y2 ) ls.y1 to ls.y2 else ls.y2 to ls.y1
    (if(ls.x2 < ls.x1 ) xRange.reverse else xRange).zip(if(ls.y2 < ls.y1 ) yRange.reverse else yRange).toList
  }
  private def xyRange(a:Int, b:Int):Range = if(a <= b) a to b else b to a
  //can i do this in an immutable way?
  def createDiagramFromLines(lines: List[LineSegment], includeDiagonal: Boolean = false, size: Int = 9 ): Diagram = {
    val diagram: Diagram = CreateDiagram(size)

    val linePoints: List[(Int,Int)] = lines.flatMap( ls ⇒ ls match
      case l if l.x1 == l.x2 ⇒ xyRange(l.y1, l.y2).map( y ⇒ (l.x1,y)).toList
      case l if l.y1 == l.y2 ⇒ xyRange(l.x1,l.x2).map( x ⇒ (x,l.y1)).toList
      case l ⇒ if(includeDiagonal){
        if ((l.y2-l.y1).abs/(l.x2 - l.x1).abs == 1) createDiagonalPoints(l)
        else Nil
      }else Nil //(y2 - y1) / (x2 - x1) == 1
    )

    linePoints.foldLeft(diagram)( (d,p) ⇒ {
      val(x,y) = p
      val point: DiagramPoint = d(y)(x)
      val newPoint: DiagramPoint = point.copy(count = point.count + 1)
      d.updated(y,d(y).updated(x,newPoint))
    })

  }

  def prettyPrintDiagram(diagram: Diagram): Unit = diagram.foreach(line ⇒ {
    line.foreach( point ⇒ if(point.count == 0) print(".") else print(point.count))
    println
  })

}
