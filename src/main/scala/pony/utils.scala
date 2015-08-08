package pony

import bwapi.Game

class Renderer(game: Game, private var color: bwapi.Color) {
  def drawTextOnScreen(text: String): Unit = {
    game.drawTextScreen(10, 10, text)
  }

  def drawOutline(where: Area): Unit = {
    drawOutline(where.upperLeft.mapX, where.upperLeft.mapY, where.lowerRight.mapX, where.lowerRight.mapY)
  }

  def drawOutline(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    game.drawBoxMap(x1, y1, x2, y2, color)
  }

  def drawTextAtUnit(u: Mobile, text: String, lineOffset: Int = 0): Unit = {
    game.drawTextMap(u.currentPositionNative.getX, u.currentPositionNative.getY + lineOffset * 10, text)
  }

  def indicateTarget(currentPosition: MapPosition, to: MapTilePosition): Unit = {
    game.drawLineMap(currentPosition.x, currentPosition.y, to.mapX, to.mapY, color)
    drawCircleAroundTile(to)
  }
  def drawCircleAroundTile(around: MapTilePosition): Unit = {
    game.drawCircleMap(around.mapX + tileSize / 2, around.mapX + tileSize / 2, tileSize / 2, color)
  }
  def indicateTarget(currentPosition: MapPosition, area: Area): Unit = {
    game.drawLineMap(currentPosition.x, currentPosition.y, area.center.x, area.center.y, color)
    markTarget(area)
  }
  def markTarget(area: Area): Unit = {
    val center = area.center
    val radius = (area.sizeOfArea.x + area.sizeOfArea.y) / 2
    game.drawCircleMap(center.x, center.y, radius, color)
  }
  def writeText(position: MapTilePosition, msg: Any): Unit = {
    game.drawTextMap(position.x * 32, position.y * 32, msg.toString)
  }

  def in_!(color: bwapi.Color) = {
    this.color = color
    this
  }

  def drawCrossedOutOnTile(x: Int, y: Int): Unit = {
    drawCrossedOutOnTile(MapTilePosition.shared(x, y))
  }

  def drawCrossedOutOnTile(p: MapTilePosition): Unit = {
    game.drawBoxMap(p.x * 32, p.y * 32, p.x * 32 + tileSize, p.y * 32 + tileSize, color)
    game.drawLineMap(p.x * 32, p.y * 32, p.x * 32 + tileSize, p.y * 32 + tileSize, color)
    game.drawLineMap(p.x * 32 + tileSize, p.y * 32, p.x * 32, p.y * 32 + tileSize, color)
  }
}

class LazyVal[T](gen: => T) {
  private var loaded   = false
  private var value: T = _
  def get = {
    if (!loaded)
      value = gen

    loaded = true
    value
  }

  def invalidate(): Unit = {
    loaded = false
    value = null.asInstanceOf[T]
  }
}

object LazyVal {
  def from[T](t: => T) = new LazyVal(t)
}
