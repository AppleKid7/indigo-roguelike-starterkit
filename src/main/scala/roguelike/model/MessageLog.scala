package roguelike.model

import indigo.lib.roguelike.DfTiles
import indigo.lib.roguelike.terminal.{MapTile, TerminalEmulator, TerminalEntity}
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.RGBA
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import indigo.lib.roguelike.terminal.MapTile
import indigo.lib.roguelike.DfTiles
import roguelike.Assets
import roguelike.RogueLikeGame

final case class Message(text: String, fgColor: RGB, count: Int, stackable: Boolean):
  def fullText: String =
    if count > 1 then s"$text (x$count)"
    else text

  def withText(newText: String): Message =
    this.copy(text = newText)

  def withFgColor(newColor: RGB): Message =
    this.copy(fgColor = newColor)

  def withCount(newCount: Int): Message =
    this.copy(count = newCount)
  def increaseCount: Message =
    withCount(count + 1)
  def decreaseCount: Message =
    withCount(count - 1)

  def makeStackable: Message =
    this.copy(stackable = true)

  def unStackable: Message =
    this.copy(stackable = false)

object Message:
  def apply(text: String, fgColor: RGB): Message =
    Message(text, fgColor, 1, true)

final case class MessageLog(messages: List[Message], maxLength: Option[Int]):
  def logLength: Int =
    messages.length

  def addMessage(message: Message): MessageLog =
    if message.stackable then
    this.copy(
        messages = messages.headOption
        .map { m =>
            val msgs =
            if m.text == message.text then m.increaseCount :: messages.tail
            else message :: m :: messages.tail

            maxLength match
            case None      => msgs
            case Some(max) => msgs.take(max)
        }
        .getOrElse(List(message))
    )
    else this.copy(messages = message :: messages)
  
  def withMaxLength(newMax: Int): MessageLog =
    this.copy(maxLength = Option(newMax))
  def noLimit: MessageLog =
    this.copy(maxLength = None)

  def toTerminal(size: Size, reversed: Boolean, startOffset: Int, fadeOut: Boolean): TerminalEmulator =
    MessageLog.logToTerminal(size, messages, reversed, startOffset, fadeOut)

  def render(position: Point, size: Size, reversed: Boolean, startOffset: Int): TerminalEntity =
    MessageLog.renderMessages(position * RogueLikeGame.charSize.toPoint, size * RogueLikeGame.charSize, messages, reversed, startOffset)

object MessageLog:

  def Unlimited: MessageLog =
    MessageLog(Nil, None)

  def Limited(maxLength: Int): MessageLog =
    MessageLog(Nil, Option(maxLength))

  def logToTerminal(size: Size, messages: List[Message], reversed: Boolean, startOffset: Int, fadeOut: Boolean): TerminalEmulator =
    val msgs = (if reversed then messages.reverse else messages).drop(startOffset)
    msgs
      .take(size.height)
      .foldLeft((TerminalEmulator(size, RogueLikeGame.maxNumberOfTiles), 0)) { case ((t, r), m) =>
        val darkenAmount = if fadeOut then (0.8 * (r.toDouble / size.height.toDouble)) + 0.2 else 0.0
        (t.putLine(Point(0, r), m.fullText, m.fgColor.mix(RGB.Black, darkenAmount), RGBA.Black), r + 1)
      }
      ._1

  def renderMessages(position: Point, size: Size, messages: List[Message], reversed: Boolean, startOffset: Int): TerminalEntity =
    logToTerminal(size, messages, reversed, startOffset, true)
      .draw(Assets.tileMap, RogueLikeGame.charSize, MapTile(DfTiles.Tile.SPACE))
      .moveTo(position)