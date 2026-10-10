package pony
package combat

trait CastOn

case object OwnUnits extends CastOn

case object EnemyUnits extends CastOn
