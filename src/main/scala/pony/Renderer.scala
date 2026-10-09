package pony

import bwapi.Game

class Renderer(g: Game, private var color: bwapi.Color) {
  private def game = {
    assert(ok)
    g
  }

  private var ok = false

  def allow() = {
    ok = true
  }

  def disallow() = {
    ok = false
  }

  private var line = 0

  def beforeTick() = {
    line = 0
  }

  def drawLine(from: MapTilePosition, to: MapTilePosition): Unit = {
    game.drawLineMap(from.mapX, from.mapY, to.mapX, to.mapY, color)
  }

  def drawLineInTile(from: MapTilePosition, to: MapTilePosition): Unit = {
    game.drawLineMap(from.mapX + 16, from.mapY + 16, to.mapX + 16, to.mapY + 16, color)
  }

  def drawStar(where: MapTilePosition, size: Int = 3): Unit = {
    game.drawLineMap(
      where.movedBy(-size, -size).nativeMapPosition,
      where.movedBy(size, size).nativeMapPosition,
      color
    )
    game.drawLineMap(
      where.movedBy(size, -size).nativeMapPosition,
      where.movedBy(-size, size).nativeMapPosition,
      color
    )
    game.drawLineMap(
      where.movedBy(-size, 0).nativeMapPosition,
      where.movedBy(size, 0).nativeMapPosition,
      color
    )
    game.drawLineMap(
      where.movedBy(0, -size).nativeMapPosition,
      where.movedBy(0, size).nativeMapPosition,
      color
    )
  }

  def drawLine(from: MapPosition, to: MapPosition): Unit = {
    game.drawLineMap(from.x, from.y, to.x, to.y, color)
  }

  def drawTextOnScreen(text: String): Unit = {
    g.drawTextScreen(10, 10 + line * 10, text)
    line += 1
  }

  def drawTextAtTile(text: String, where: MapTilePosition): Unit = {
    game.drawTextMap(where.mapX, where.mapY, text)
  }

  def drawOutline(where: Area): Unit = {
    drawOutline(
      where.upperLeft.mapX,
      where.upperLeft.mapY,
      where.lowerRight.mapX + tileSize,
      where.lowerRight.mapY + tileSize
    )
  }

  def drawOutline(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    game.drawBoxMap(x1, y1, x2, y2, color)
  }

  def drawTextAtMobileUnit(u: Mobile, text: String, lineOffset: Int = 0): Unit = {
    val x = u.currentPositionNative.getX
    val y = u.currentPositionNative.getY + lineOffset * 10 + 5
    game.drawTextMap(x, y, text)
  }

  def drawTextAtStaticUnit(u: StaticallyPositioned, text: String, lineOffset: Int = 0): Unit = {
    val x = u.nativeMapPosition.getX
    val y = u.nativeMapPosition.getY + lineOffset * 10 + u.area.height * tileSize / 2 - 5
    game.drawTextMap(x, y, text)
  }

  def indicateTarget(currentPosition: MapTilePosition, to: MapTilePosition): Unit = {
    indicateTarget(currentPosition.asMapPosition, to)
  }

  def indicateTarget(currentPosition: MapPosition, to: MapTilePosition): Unit = {
    game.drawLineMap(
      currentPosition.x + tileSize / 2,
      currentPosition.y + tileSize / 2,
      to.mapX + tileSize / 2,
      to.mapY + tileSize / 2,
      color
    )
    drawCircleAroundTile(to)
  }

  def drawCircleAroundTile(around: MapTilePosition): Unit = {
    game.drawCircleMap(around.mapX + tileSize / 2, around.mapY + tileSize / 2, tileSize / 2, color)
  }

  def indicateTarget(currentPosition: MapPosition, to: MapPosition): Unit = {
    game.drawLineMap(currentPosition.x, currentPosition.y, to.x, to.y, color)
    drawCircleAround(to)
  }

  def drawCircleAround(around: MapPosition): Unit = {
    drawCircleAround(around, tileSize / 2)
  }

  def drawCircleAround(around: MapTilePosition): Unit = {
    drawCircleAround(around.asMapPosition, tileSize / 2)
  }

  def drawCircleAround(around: MapPosition, radiusPixels: Int): Unit = {
    game.drawCircleMap(around.x, around.y, radiusPixels, color)
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
