package pony

import bwapi.Game


class Renderer(game: Game, private var color: bwapi.Color) {
  def writeText(position: Point, msg: Any): Unit = {
    game.drawTextMap(position.x * 32, position.y * 32, msg.toString)
  }

  def in_!(color: bwapi.Color) = {
    this.color = color
    this
  }

  def drawCrossedOutOnTile(x: Int, y: Int): Unit = {
    drawCrossedOutOnTile(Point.shared(x, y))
  }

  def drawCrossedOutOnTile(p: Point): Unit = {
    game.drawBoxMap(p.x * 32, p.y * 32, p.x * 32 + tileSize, p.y * 32 + tileSize, color)
    game.drawLineMap(p.x * 32, p.y * 32, p.x * 32 + tileSize, p.y * 32 + tileSize, color)
    game.drawLineMap(p.x * 32 + tileSize, p.y * 32, p.x * 32, p.y * 32 + tileSize, color)
  }
}

class LazyVal[T](gen : => T) {
  private var loaded = false
  private var value:T = _
  def get = {
    if (!loaded)
      value = gen

    loaded = true
    value
  }

  def invalidate():Unit = {
    loaded = false
    value = null.asInstanceOf[T]
  }
}
