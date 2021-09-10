package roguelike.utils

import indigo._
import indigo.lib.roguelike.utils.MapUtils.bresenhamLine
import indigo.shared.datatypes.Point
import indigoextras.geometry.BoundingBox
import indigoextras.trees.QuadTree
import indigoextras.trees.QuadTree.{QuadBranch, QuadEmpty, QuadLeaf}
import roguelike.model.GameTile

import scala.annotation.tailrec

object FOV:
  def calculateFOV(radius: Int, center: Point, tileMap: QuadTree[GameTile]): List[Point] =
    val bounds: Rectangle =
      Rectangle(
        (center - radius).max(0),
        (Size(center.x, center.y) + radius).max(1)
      )
    val tiles =
      searchByBounds(tileMap, bounds, _ => true).filter(pt => center.distanceTo(pt) <= radius)

    @tailrec
    def visibleTiles(remaining: List[Point], acc: List[Point]): List[Point] =
      remaining match
        case Nil => acc
        case pt :: pts =>
          val lineOfSight = bresenhamLine(pt, center)
          if lineOfSight.forall(tiles.contains) then
            visibleTiles(
              pts,
              pt :: acc
            )
          else visibleTiles(pts, acc)
    visibleTiles(tiles, Nil)

  def searchByBounds(quadTree: QuadTree[GameTile], bounds: Rectangle, filter: GameTile => Boolean): List[Point] =
    val boundingBox: BoundingBox = BoundingBox.fromRectangle(bounds)

    @tailrec
    def rec(remaining: List[QuadTree[GameTile]], acc: List[Point]): List[Point] =
      remaining match
        case Nil => acc
        case x :: xs =>
          x match
            case QuadBranch(bounds, a, b, c, d) if boundingBox.overlaps(bounds) =>
              rec(a :: b :: c :: d :: xs, acc)
            case QuadLeaf(_, exactPosition, value) if boundingBox.contains(exactPosition) && filter(value) =>
              rec(xs, exactPosition.toPoint :: acc)
            case _ => rec(xs, acc)
    rec(List(quadTree), Nil)

  def getPathTo(dice: Dice, from: Point, to: Point, walkable: List[Point], area: Rectangle): List[Point] =
    PathFinder
      .fromWalkable(area.size, walkable.map(_ - area.position))
      .locatePath(dice, from - area.position, to - area.position, _ => 1)
      .map(_ + area.position)
