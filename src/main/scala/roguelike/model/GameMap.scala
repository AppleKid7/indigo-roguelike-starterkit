package roguelike.model

import indigo._
import indigo.lib.roguelike.terminal.MapTile
import indigo.lib.roguelike.DfTiles

import indigoextras.trees.QuadTree
import indigoextras.trees.QuadTree.{QuadBranch, QuadEmpty, QuadLeaf}
import indigoextras.geometry.Vertex

import roguelike.GameEvent

import scala.annotation.tailrec
import indigoextras.geometry.BoundingBox
import roguelike.utils.PathFinder
import roguelike.utils.FOV

final case class GameMap(
  size: Size, 
  tileMap: QuadTree[GameTile], 
  visible: List[Point], 
  explored: Set[Point], 
  entities: List[Entity]
):
  def entitiesList: List[Entity] =
    entities.filter(e => visible.contains(e.position))
  
  def damageEntity(id: Int, damage: Int): GameMap =
    this.copy(
      entities = entities.map {
        case e: Hostile if e.id == id =>
          e.takeDamage(damage)

        case e => e
      }
    )
  
  private def updateMap(tm: QuadTree[GameTile], coords: Point, f: GameTile => GameTile): QuadTree[GameTile] =
    val vtx = Vertex.fromPoint(coords)
    tm.fetchElementAt(vtx).map(f) match
      case None => tm
      case Some(tile) => tm.insertElement(tile, vtx)

  def updateEntities(dice: Dice, playerPosition: Point, pause: Boolean): Outcome[GameMap] =
    val newVisible = FOV.calculateFOV(15, playerPosition, tileMap)
    val updatedEntities =
      if !pause then
        Outcome.sequence(
          entities.map {
            case entity: Hostile =>
              entity.nextMove(dice, playerPosition, this)

            case entity => Outcome(entity)
          }
        )
      else Outcome(entities)

    updatedEntities.map { es => 
      this.copy(
        visible = newVisible,
        explored = explored ++ newVisible,
        entities = es
      )
    }
  
  def update(dice: Dice, playerPosition: Point, pause: Boolean): GlobalEvent => Outcome[GameMap] =
    case e: GameEvent.MoveEntity =>
      val updatedEntities =
        Outcome.sequence(entities.map(_.update(dice, playerPosition, this)(e)))
      
      updatedEntities.map { es =>
        this.copy(
          entities = es
        )
      }

    case _ =>
      Outcome(this)

  def visibleTiles: List[(Point, MapTile)] =
    visible
      .map(pt => lookUp(pt).map(t => (pt, t.lightMapTile)))
      .collect { case Some(mapTile) => mapTile }

  def insert(coords: Point, tile: GameTile): GameMap =
    this.copy(
      tileMap = tileMap.insertElement(tile, Vertex.fromPoint(coords))
    )

  def insert(tiles: List[(Point, GameTile)]): GameMap =
    this.copy(
      tileMap = tileMap.insertElements(tiles.map(p => (p._2, Vertex.fromPoint(p._1))))
    )
  
  def insert(tiles: (Point, GameTile)*): GameMap =
    insert(tiles.toList)

  def lookUp(at: Point): Option[GameTile] =
    tileMap.fetchElementAt(Vertex.fromPoint(at))

  def toExploredTiles: List[(Point, MapTile)] =
    @tailrec
    def rec(open: List[QuadTree[GameTile]], acc: List[(Point, MapTile)]): List[(Point, MapTile)] =
      open match
        case Nil => acc
        case x :: xs =>
          x match
            case _: QuadEmpty[GameTile] => rec(xs, acc)
            case l: QuadLeaf[GameTile] if explored.contains(l.exactPosition.toPoint) =>
              rec(xs, (l.exactPosition.toPoint, l.value.darkMapTile) :: acc)
            case l: QuadLeaf[GameTile] =>
              rec(xs, acc)
            case b: QuadBranch[GameTile] if b.isEmpty =>
              rec(xs, acc)
            case QuadBranch(_, a, b, c, d) =>
              val next =
                (if a.isEmpty then Nil else List(a)) ++
                (if b.isEmpty then Nil else List(b)) ++
                (if c.isEmpty then Nil else List(c)) ++
                (if d.isEmpty then Nil else List(d))
              
              rec(xs ++ next, acc)
          end match
      end match
    end rec
    rec(List(tileMap), Nil)
  end toExploredTiles

  def getPathTo(dice: Dice, from: Point, to: Point, additionalBlocked: List[Point]) =
    val area = Rectangle.fromTwoPoints(from, to).expand(2)
    val filter: GameTile => Boolean = {
      case GameTile.Ground => true
      case _ => false
    }
    val walkable = FOV.searchByBounds(tileMap, area, filter).filterNot(additionalBlocked.contains)

    FOV.getPathTo(dice, from, to, walkable, area)

object GameMap:
  def initial(size: Size, entities: List[Entity]): GameMap =
    GameMap(
      size,
      QuadTree.empty(size.width, size.height),
      Nil,
      Set(),
      entities
    )

  def generateMap(size: Size, dungeon: Dungeon): GameMap =
    initial(size, dungeon.entities).insert(dungeon.positionedTiles)
