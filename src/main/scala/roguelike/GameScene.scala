package roguelike

import indigo._
import indigo.scenes._

import indigo.lib.roguelike.DfTiles
import indigo.lib.roguelike.terminal.{TerminalEmulator, MapTile, TerminalEntity, TerminalText}

import roguelike.model.{Action, Model, ViewModel}
import _root_.io.circe.CursorOp.MoveUp
import roguelike.RogueLikeGame.RegenerateLevel
import roguelike.model.GameTile

object GameScene extends Scene[Unit, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = ViewModel

  val name: SceneName =
    SceneName("game scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(context: FrameContext[Unit], model: Model): GlobalEvent => Outcome[Model] =
    case KeyboardEvent.KeyUp(Key.SPACE) =>
      Outcome(model).addGlobalEvents(SceneEvent.JumpTo(StartScene.name))
    case KeyboardEvent.KeyUp(Key.UP_ARROW) =>
      Outcome(model.copy(player = model.player.handleAction(Action.MoveUp, model.gameMap)))
    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) =>
      Outcome(model.copy(player = model.player.handleAction(Action.MoveDown, model.gameMap)))
    case KeyboardEvent.KeyUp(Key.LEFT_ARROW) =>
      Outcome(model.copy(player = model.player.handleAction(Action.MoveLeft, model.gameMap)))
    case KeyboardEvent.KeyUp(Key.RIGHT_ARROW) =>
      Outcome(model.copy(player = model.player.handleAction(Action.MoveRight, model.gameMap)))
    case RegenerateLevel =>
      Outcome(Model.generateDungeon(context.dice, model.screenSize))
    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case RegenerateLevel =>
      Outcome(
        viewModel.copy(
          background = TerminalEmulator(RogueLikeGame.screenSize)
            .put(model.gameMap.toPositionedTiles)
        )
      )
    case _ => Outcome(viewModel)

  def present(context: FrameContext[Unit], model: Model, viewModel: ViewModel): Outcome[SceneUpdateFragment] =
    if model.gameMap.tileMap.isEmpty then
      Outcome(
        SceneUpdateFragment(
          TextBox("No level", 100, 30)
            .withColor(RGBA.White)
            .withFontFamily(FontFamily.monospace)
        )
      )
    else
      val entities =
        TerminalEmulator(model.screenSize)
          .put(model.entitiesList.map(e => (e.position, e.tile)))
      Outcome(
        SceneUpdateFragment(
          viewModel.background
            .combine(entities)
            .draw(Assets.tileMap, RogueLikeGame.charSize, GameTile.DarkWall.mapTile)
        )
      )
