package roguelike.model

import indigo._

import indigo.lib.roguelike.DfTiles
import indigo.lib.roguelike.terminal.TerminalEntity
import indigo.lib.roguelike.terminal.MapTile

final case class ViewModel(terminalEntity: Option[TerminalEntity], shroud: MapTile)

object ViewModel {
  def initial(screenSize: Size): ViewModel =
    ViewModel(None, MapTile(DfTiles.Tile.SPACE))
}
