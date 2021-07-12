package roguelike.model

import indigo._
import _root_.io.circe.CursorOp.MoveUp
import indigo.lib.roguelike.terminal.MapTile
import indigo.lib.roguelike.DfTiles
import indigoextras.geometry.Vertex
import indigoextras.trees.QuadTree
import indigoextras.trees.QuadTree.{QuadBranch, QuadEmpty, QuadLeaf}
import scala.annotation.tailrec

final case class Model(screenSize: Size, player: Player, entities: List[Entity], gameMap: GameMap):
  def entitiesList: List[Entity] = player :: entities
  
object Model:
  def initial(screenSize: Size): Model =
    Model(
      screenSize, 
      Player.initial(screenSize),
      List(
        NPC((screenSize.toPoint / 2) + Point(-5))
      ),
      GameMap(screenSize, QuadTree.empty(screenSize.width, screenSize.height))
    )

  def generateDungeon(dice: Dice, screenSize: Size): Model =
    val dungeon = DungeonGenerator.makeMap(dice, 30, 6, 10, screenSize)
    Model(
      screenSize,
      Player(dungeon.playerStart),
      List(
        NPC((screenSize.toPoint / 2) + Point(-5))
      ),
      GameMap.initial(screenSize, dungeon)
    )

sealed trait Entity:
  def position: Point
  def tile: MapTile

  def moveBy(amount: Point, gameMap: GameMap): Entity

  def moveBy(x: Int, y: Int, gameMap: GameMap): Entity =
    moveBy(Point(x, y), gameMap)

final case class Player(position: Point) extends Entity:
  val tile: MapTile = MapTile(DfTiles.Tile.`@`, RGB.fromHexString("4b7751"))

  def moveBy(amount: Point, gameMap: GameMap): Player =
    gameMap.lookUp(position + amount) match
      case None =>
        this

      case Some(tile) if tile.isBlocked =>
        this

      case Some(tile) =>
        this.copy(position = position + amount)

  def handleAction(action: Action, gameMap: GameMap): Player =
    action match
      case Action.MoveUp => moveBy(Point(0, -1), gameMap)
      case Action.MoveDown => moveBy(Point(0, 1), gameMap)
      case Action.MoveLeft => moveBy(Point(-1, 0), gameMap)
      case Action.MoveRight => moveBy(Point(1, 0), gameMap)

object Player:
  def initial(screenSize: Size): Player =
    Player(screenSize.toPoint / 2)

enum Action:
  case MoveUp
  case MoveDown
  case MoveLeft
  case MoveRight

final case class NPC(position: Point) extends Entity:
  val tile: MapTile = MapTile(DfTiles.Tile.WHITE_SMILING_FACE, RGB.Cyan)

  def moveBy(amount: Point, gameMap: GameMap): NPC =
    this.copy(position = position + amount)

enum GameTile(val mapTile: MapTile, val blocked: Boolean, val blockSight: Boolean):
  def isBlocked: Boolean = this.blocked
  case DarkWall extends GameTile(
    mapTile = MapTile(DfTiles.Tile.DARK_SHADE, RGB(0.9, 0, 0.0)),
    blocked = true,
    blockSight = true
  )
  case Ground extends GameTile(
    mapTile = MapTile(DfTiles.Tile.LIGHT_SHADE, RGB(0.1, 0.1, 0.4)),
    blocked = false,
    blockSight = false
  )

final case class GameMap(size: Size, tileMap: QuadTree[GameTile]):
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

  def inBounds(point: Point): Boolean =
    return (0 <= point.x && point.x < size.width) && (0 <= point.y && point.y < size.height)

  def lookUp(at: Point): Option[GameTile] =
    tileMap.fetchElementAt(Vertex.fromPoint(at))

  def toPositionedTiles: List[(Point, MapTile)] =
    @tailrec
    def rec(open: List[QuadTree[GameTile]], acc: List[(Point, MapTile)]): List[(Point, MapTile)] =
      open match
        case Nil => acc
        case x :: xs =>
          x match
            case _: QuadEmpty[GameTile] => rec(xs, acc)
            case l: QuadLeaf[GameTile] =>
              rec(xs, (l.exactPosition.toPoint, l.value.mapTile) :: acc)
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
  end toPositionedTiles

object GameMap:
  def initial(size: Size, dungeon: Dungeon): GameMap =
    GameMap(
      size,
      QuadTree.empty(size.width, size.height)
    ).insert(dungeon.positionedTiles)
