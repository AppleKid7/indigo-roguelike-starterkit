package roguelike.model

import indigo._
import indigoextras.trees.QuadTree

final case class Model(screenSize: Size, player: Player, gameMap: GameMap, message: String):
  def entitiesList: List[Entity] =
    player :: gameMap.entitiesList

  def moveUp: Model =
    val p = player.bump(Point(0, -1), gameMap)
    this.copy(
      player = p.player,
      gameMap = gameMap.update(p.player.position),
      message = p.message
    )

  def moveDown: Model =
    val p = player.bump(Point(0, 1), gameMap)
    this.copy(
      player = p.player,
      gameMap = gameMap.update(p.player.position),
      message = p.message
    )

  def moveLeft: Model =
    val p = player.bump(Point(-1, 0), gameMap)
    this.copy(
      player = p.player,
      gameMap = gameMap.update(p.player.position),
      message = p.message
    )

  def moveRight: Model =
    val p = player.bump(Point(1, 0), gameMap)
    this.copy(
      player = p.player,
      gameMap = gameMap.update(p.player.position),
      message = p.message
    )
  
object Model:
  def initial(screenSize: Size): Model =
    Model(
      screenSize, 
      Player(Point.zero),
      GameMap.initial(screenSize, Nil),
      ""
    )

  def generateModel(dice: Dice, screenSize: Size): Model =
    val dungeon = DungeonGenerator.makeMap(
      dice,
      DungeonGenerator.maxRooms,
      DungeonGenerator.roomMinSize,
      DungeonGenerator.roomMaxSize,
      screenSize,
      DungeonGenerator.maxMonstersPerRoom
    )
    Model(
      screenSize,
      Player(dungeon.playerStart),
      GameMap.generateMap(screenSize, dungeon).update(dungeon.playerStart),
      ""
    )
