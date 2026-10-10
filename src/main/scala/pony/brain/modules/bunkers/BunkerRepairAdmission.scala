package pony
package brain
package modules
package bunkers

private[pony] object BunkerRepairAdmission {
  def eligible(
      damaged: Boolean,
      completed: Boolean,
      alive: Boolean,
      local: Boolean,
      mineralOrIdle: Boolean
  ) = damaged && completed && alive && local && mineralOrIdle
}
