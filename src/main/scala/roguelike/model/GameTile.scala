package roguelike.model

import indigo._
import indigo.lib.roguelike.terminal.MapTile
import indigo.lib.roguelike.utils.GameTileInterface
import indigo.lib.roguelike.DfTiles

enum GameTile(val lightMapTile: MapTile, val darkMapTile: MapTile, val blocked: Boolean, val blockSight: Boolean) extends GameTileInterface:
  case Wall extends GameTile(
    lightMapTile = MapTile(DfTiles.Tile.DARK_SHADE, RGB(0.9, 0.1, 0.1)),
    darkMapTile = MapTile(DfTiles.Tile.DARK_SHADE, RGB(0.4, 0.1, 0.1)),
    blocked = true,
    blockSight = true
  )
  case Ground extends GameTile(
    lightMapTile = MapTile(DfTiles.Tile.LIGHT_SHADE, RGB(1.0, 1.0, 0.0), RGBA(0.75, 0.6, 0.3, 1.0)),
    darkMapTile = MapTile(DfTiles.Tile.LIGHT_SHADE, RGB(0.0, 0.4, 1.0), RGBA(0.0, 0.0, 0.5, 1.0)),
    blocked = false,
    blockSight = false
  )

  val scoreAs: GameTile => Int = {
    case Ground => 1
    case Wall => Int.MaxValue
  }
