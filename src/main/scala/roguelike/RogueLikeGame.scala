package roguelike

import indigo._
import indigo.scenes._
import scala.scalajs.js.annotation.JSExportTopLevel

import indigo.lib.roguelike.DfTiles
import indigo.lib.roguelike.terminal.{TerminalEntity, TerminalText}
import roguelike.model.Model
import roguelike.model.ViewModel

@JSExportTopLevel("IndigoGame")
object RogueLikeGame extends IndigoGame[Unit, Unit, Model, ViewModel]:

  val screenSize: Size = Size(80, 50)
  val charSize: Size = Size(10, 10)
  val maxNumberOfTiles: Int = 4096

  def initialScene(bootData: Unit): Option[SceneName] =
    None

  def scenes(bootData: Unit): NonEmptyList[Scene[Unit, Model, ViewModel]] =
    NonEmptyList(GameScene)

  val eventFilters: EventFilters =
    EventFilters.Permissive

  def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome(
      BootResult
        .noData(
          GameConfig.default
            .withMagnification(1)
            .withFrameRate(30)
            .withViewport(screenSize.width * charSize.width, screenSize.height * charSize.height)
        )
        .withFonts(DfTiles.Fonts.fontInfo)
        .withAssets(Assets.assets)
        .withShaders(
          TerminalEntity.shader(Assets.Required.mapFragShader),
          TerminalText.shader(Assets.Required.textFragShader)
        )
    ).addGlobalEvents(RegenerateLevel)

  def initialModel(startupData: Unit): Outcome[Model] =
    Outcome(Model.initial(screenSize))

  def initialViewModel(startupData: Unit, model: Model): Outcome[ViewModel] =
    Outcome(ViewModel.initial)

  def setup(bootData: Unit, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  def updateModel(context: FrameContext[Unit], model: Model): GlobalEvent => Outcome[Model] =
    _ => Outcome(model)

  def updateViewModel(context: FrameContext[Unit], model: Model, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome(viewModel)

  def present(context: FrameContext[Unit], model: Model, viewModel: ViewModel): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment(
      Layer(BindingKey("game")),
      Layer(BindingKey("log")),
      Layer(BindingKey("fps"))
    ))

  case object RegenerateLevel extends GlobalEvent
