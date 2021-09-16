package roguelike

import indigo._
import indigo.scenes._

import indigo.lib.roguelike.DfTiles
import indigo.lib.roguelike.terminal.{TerminalEmulator, TerminalEntity, TerminalText}
import roguelike.model.{Model, ViewModel}

object StartScene extends Scene[Unit, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = ViewModel

  val name: SceneName =
    SceneName("start scene")

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
      Outcome(model).addGlobalEvents(SceneEvent.JumpTo(GameScene.name))
    case GameEvent.RegenerateLevel =>
      Model.generateModel(context.dice, model.screenSize)
    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case GameEvent.RegenerateLevel =>
      val term =
        TerminalEmulator(RogueLikeGame.screenSize, RogueLikeGame.maxNumberOfTiles)
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

  val size = Size(30)

  def message: String =
    """
    |╔═════════════════════╗
    |║ Hit Space to Start! ║
    |╚═════════════════════╝
    |""".stripMargin

  def present(context: FrameContext[Unit], model: Model, viewModel: ViewModel): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Text(message, DfTiles.Fonts.fontKey, TerminalText(Assets.tileMap, RGB.Cyan, RGBA.Blue))
      )
    )
