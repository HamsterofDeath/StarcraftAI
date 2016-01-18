package pony

import bwapi.{Player, Position}

import scala.collection.mutable.ArrayBuffer

trait WorldListener {
  def onNukeDetect(unit: Position): Unit = {}

  def onUnitDestroy(unit: bwapi.Unit): Unit = {}

  def onUnitMorph(unit: bwapi.Unit): Unit = {}

  def onUnitRenegade(unit: bwapi.Unit): Unit = {}

  def onPlayerLeft(player: Player): Unit = {}

  def onUnitHide(unit: bwapi.Unit): Unit = {}

  def onPlayerDropped(player: Player): Unit = {}

  def onUnitComplete(unit: bwapi.Unit): Unit = {}

  def onUnitEvade(unit: bwapi.Unit): Unit = {}

  def onUnitDiscover(unit: bwapi.Unit): Unit = {}

  def onUnitShow(unit: bwapi.Unit): Unit = {}

  def onUnitCreate(unit: bwapi.Unit): Unit = {}
}

trait WorldEventDispatcher extends WorldListener {
  private val receivers = ArrayBuffer.empty[WorldListener]

  def listen_!(receiver: WorldListener): Unit = {
    receivers += receiver
  }
  override def onNukeDetect(unit: Position): Unit = {
    super.onNukeDetect(unit)
    receivers.foreach(_.onNukeDetect(unit))
  }
  override def onUnitDestroy(unit: bwapi.Unit): Unit = {
    super.onUnitDestroy(unit)
    receivers.foreach(_.onUnitDestroy(unit))
  }
  override def onUnitMorph(unit: bwapi.Unit): Unit = {
    super.onUnitMorph(unit)
    receivers.foreach(_.onUnitMorph(unit))
  }
  override def onUnitRenegade(unit: bwapi.Unit): Unit = {
    super.onUnitRenegade(unit)
    receivers.foreach(_.onUnitRenegade(unit))
  }
  override def onPlayerLeft(player: Player): Unit = {
    super.onPlayerLeft(player)
    receivers.foreach(_.onPlayerLeft(player))
  }
  override def onUnitHide(unit: bwapi.Unit): Unit = {
    super.onUnitHide(unit)
    receivers.foreach(_.onUnitHide(unit))
  }
  override def onPlayerDropped(player: Player): Unit = {
    super.onPlayerDropped(player)
    receivers.foreach(_.onPlayerDropped(player))
  }
  override def onUnitComplete(unit: bwapi.Unit): Unit = {
    super.onUnitComplete(unit)
    receivers.foreach(_.onUnitComplete(unit))
  }
  override def onUnitEvade(unit: bwapi.Unit): Unit = {
    super.onUnitEvade(unit)
    receivers.foreach(_.onUnitEvade(unit))
  }
  override def onUnitDiscover(unit: bwapi.Unit): Unit = {
    super.onUnitDiscover(unit)
    receivers.foreach(_.onUnitDiscover(unit))
  }
  override def onUnitShow(unit: bwapi.Unit): Unit = {
    super.onUnitShow(unit)
    receivers.foreach(_.onUnitShow(unit))
  }
  override def onUnitCreate(unit: bwapi.Unit): Unit = {
    super.onUnitCreate(unit)
    receivers.foreach(_.onUnitCreate(unit))
  }
}