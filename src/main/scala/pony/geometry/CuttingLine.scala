package pony
package geometry

case class CuttingLine(line: Line) {
  def center = line.center

  def absoluteFrom = line.a

  def absoluteTo = line.b
}
