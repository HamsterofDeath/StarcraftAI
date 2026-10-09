package pony
package brain
package modules

private[pony] object BunkerRepairAdmission {
  def eligible(
      damaged: Boolean,
      completed: Boolean,
      alive: Boolean,
      local: Boolean,
      mineralOrIdle: Boolean
  ) = damaged && completed && alive && local && mineralOrIdle
}
