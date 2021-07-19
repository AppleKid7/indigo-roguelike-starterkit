package roguelike.model

import indigo._
import indigo.lib.roguelike.DfTiles
import indigo.lib.roguelike.terminal.MapTile

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
