package de.bjrke.euler

/**
 * Lattice paths
 * 
 * Starting in the top left corner of a 2×2 grid, and only being able to move
 * to the right and down, there are exactly 6 routes to the bottom right corner.
 *
 * How many such routes are there through a 20×20 grid?
 */
class Problem0015 extends Problem[Long] {

  override val result = 137846528820L

  override def apply = {
    val p = Position(20,20)
    count( Map() + ( Position(0,0) -> 1L), p ).get( p ).get
  }

  def count( map: Map[Position, Long], pos: Position ) : Map[Position, Long] = {
    if ( map.contains(pos) ) {
      map
    } else if ( pos.x < 0 || pos.y < 0 ) {
      map + ( pos -> 0L )
    } else {
      val newMap = List( pos.down, pos.right ).foldLeft(map) { (m,p) => count(m, p)}
      newMap + ( pos -> ( newMap.get( pos.right ).get + newMap.get( pos.down ).get ) )
    }
  }

  case class Position( x: Int, y: Int ) {
    private val min = Math.min(x,y)
    private val max = Math.max(x,y)
    override def hashCode = 31 + min.hashCode + 2 * max.hashCode
    override def equals( a: Any ) = a match {
      case p : Position => p.min == min && p.max == max
      case _ => false
    }
    lazy val down = Position( x, y - 1 )
    lazy val right = Position( x - 1, y )
  }

}
