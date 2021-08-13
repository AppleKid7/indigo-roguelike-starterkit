package roguelike.model

import indigo._
import indigo.lib.roguelike.DfTiles
import indigo.lib.roguelike.terminal.MapTile

sealed trait Entity:
  def position: Point
  def tile: MapTile
  def blocksMovement: Boolean
  def name: String

  def moveBy(amount: Point, gameMap: GameMap): Entity

  def moveBy(x: Int, y: Int, gameMap: GameMap): Entity =
    moveBy(Point(x, y), gameMap)

final case class Player(position: Point) extends Entity:
  val tile: MapTile = MapTile(DfTiles.Tile.`@`, RGB.fromHexString("4b7751"))
  val blocksMovement: Boolean = false
  val name: String = "Player"

  def bump(amount: Point, gameMap: GameMap): PlayerUpdate =
    gameMap.entities.find(e => e.position == position + amount && e.blocksMovement) match
      case None => PlayerUpdate(moveBy(amount, gameMap), "")
      case Some(target) => PlayerUpdate(this, s"You kick the ${target.name}, much to its annoyance!")

  def moveBy(amount: Point, gameMap: GameMap): Player =
    gameMap.lookUp(position + amount) match
      case None =>
        this

      case Some(tile) if tile.isBlocked =>
        this

      case Some(tile) =>
        this.copy(position = position + amount)



final case class Orc(position: Point) extends Entity:
  val tile: MapTile = MapTile(DfTiles.Tile.`o`, RGB.fromColorInts(63, 127, 63))
  val blocksMovement: Boolean = true
  val name: String = "Orc"

  def moveBy(amount: Point, gameMap: GameMap): Orc =
    this.copy(position = position + amount)

final case class Troll(position: Point) extends Entity:
  val tile: MapTile = MapTile(DfTiles.Tile.`T`, RGB.fromColorInts(0, 127, 0))
  val blocksMovement: Boolean = true
  val name: String = "Troll"

  def moveBy(amount: Point, gameMap: GameMap): Troll =
    this.copy(position = position + amount)

final case class PlayerUpdate(player: Player, message: String)
enum PlayerAction derives CanEqual:
  case MoveNorth, MoveSouth, MoveEast, MoveWest, Escape, Bump
