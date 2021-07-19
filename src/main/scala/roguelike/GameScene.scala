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
      Outcome(model.moveUp)
    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) =>
      Outcome(model.moveDown)
    case KeyboardEvent.KeyUp(Key.LEFT_ARROW) =>
      Outcome(model.moveLeft)
    case KeyboardEvent.KeyUp(Key.RIGHT_ARROW) =>
      Outcome(model.moveRight)
    case RegenerateLevel =>
      Outcome(Model.generateModel(context.dice, model.screenSize))
    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case KeyboardEvent.KeyUp(_) | RegenerateLevel =>
      val term =
        TerminalEmulator(RogueLikeGame.screenSize)
          .put(model.gameMap.toExploredTiles)
          .put(model.gameMap.visibleTiles)
          .put(model.entitiesList.map(e => (e.position, e.tile)))
          .draw(Assets.tileMap, RogueLikeGame.charSize, viewModel.shroud)
      Outcome(
        viewModel.copy(
          terminalEntity = Option(term)
        )
      )
    case _ => Outcome(viewModel)

  def present(context: FrameContext[Unit], model: Model, viewModel: ViewModel): Outcome[SceneUpdateFragment] =
    viewModel.terminalEntity match
      case None =>
        Outcome(
          SceneUpdateFragment(
            Layer(
              BindingKey("game"),
              TextBox("No level", 100, 30)
                .withColor(RGBA.White)
                .withFontFamily(FontFamily.monospace)
            )
          )
        )
      case Some(entity) =>
        Outcome(
          SceneUpdateFragment(
            Layer(
              BindingKey("game"),
              entity
            )
          )
        )
