package com.xmas.challenge.twenty20

import scala.io.Source
import scala.util.Try

/**
 * --- Day 3: Toboggan Trajectory ---
 */
object Day3 {
  private lazy val day3File: String = "Day3_2020_Input.txt"
  lazy val defaultInput: List[String] = Source.fromResource(day3File).getLines.toList


  trait Space
  case object Tree extends Space
  case object Free extends Space
  case object Done extends Space

  type Row = Vector[Space]
  type TreeMatrix = Vector[Row]

  case class Location(over:Int, down:Int)
  case class Slope(over:Int, down: Int)

  lazy val defaultSlopes: List[Slope] = List(Slope(1,1), Slope(3,1), Slope(5,1), Slope(7,1), Slope(1,2) )
  lazy val defaultTreeMatrix: TreeMatrix = parseMatrix(defaultInput)

  def parseMatrix(input: List[String]): TreeMatrix =
    input.map( inputRow ⇒
      inputRow.map( _ match {
        case '.' ⇒ Free
        case '#' ⇒ Tree
      } ).toVector
    ).toVector

  def calculateNextLocationOnMatrix(location: Location, slope: Slope, treeMatrix: TreeMatrix): Option[Location] = {

    val newLocation = location.copy(location.over + slope.over, location.down + slope.down)
    val maxOver = treeMatrix.head.length

    if(newLocation.down >= treeMatrix.size) None //todo what if we're not at the bottom yet?
    else if(newLocation.over >= maxOver) {
      Some(newLocation.copy(over = newLocation.over - maxOver))
    }
    else Some(newLocation)
  }

  def getSpaceFromMatrix(location: Location, treeMatrix: TreeMatrix): Space = {
    val maxOver = treeMatrix.head.length
    if(location.down > treeMatrix.size) Done
    else if(location.over > maxOver) Done
    else treeMatrix(location.down)(location.over)
  }

  def accumulateTreeCount(space: Space, treeCount: Int): Int = space match {
    case Tree ⇒ treeCount + 1
    case _ ⇒ treeCount
  }

  def countTreesOnRoute(initLocation: Location, slope: Slope, treeMatrix: TreeMatrix, numTrees: Int): Long = calculateNextLocationOnMatrix(initLocation, slope, treeMatrix) match {
    case Some(nextLocation) ⇒ countTreesOnRoute(nextLocation, slope, treeMatrix, accumulateTreeCount(getSpaceFromMatrix(initLocation, treeMatrix) , numTrees))
    case None ⇒ accumulateTreeCount(getSpaceFromMatrix(initLocation, treeMatrix) , numTrees)
  }


  def countTreesOnRouteDefault: Long = countTreesOnRoute(Location(0,0),Slope(3,1),defaultTreeMatrix,0)

  def computePathOnMultipleRoutesDefault: Long = defaultSlopes.map( slope ⇒ countTreesOnRoute(Location(0,0),slope, defaultTreeMatrix,0)).product

}
