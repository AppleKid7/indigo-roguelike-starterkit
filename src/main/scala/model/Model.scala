package roguelike.model

import indigo._
import indigoextras.trees.QuadTree

final case class Model(screenSize: Size, player: Player, entities: List[Entity], gameMap: GameMap):
  def entitiesList: List[Entity] =
    player :: entities.filter(e => gameMap.visible.contains(e.position))

  def moveUp: Model =
    val p = player.handleAction(Action.MoveUp, gameMap)
    this.copy(
      player = p,
      gameMap = gameMap.update(p.position)
    )

  def moveDown: Model =
    val p = player.handleAction(Action.MoveDown, gameMap)
    this.copy(
      player = p,
      gameMap = gameMap.update(p.position)
    )

  def moveLeft: Model =
    val p = player.handleAction(Action.MoveLeft, gameMap)
    this.copy(
      player = p,
      gameMap = gameMap.update(p.position)
    )

  def moveRight: Model =
    val p = player.handleAction(Action.MoveRight, gameMap)
    this.copy(
      player = p,
      gameMap = gameMap.update(p.position)
    )
  
object Model:
  def initial(screenSize: Size): Model =
    Model(
      screenSize, 
      Player(Point.zero),
      Nil,
      GameMap.initial(screenSize)
    )

  def generateModel(dice: Dice, screenSize: Size): Model =
    val dungeon = DungeonGenerator.makeMap(dice, 30, 6, 10, screenSize)
    Model(
      screenSize,
      Player(dungeon.playerStart),
      List(
        NPC((screenSize.toPoint / 2) + Point(-5))
      ),
      GameMap.generateMap(screenSize, dungeon).update(dungeon.playerStart)
    )
