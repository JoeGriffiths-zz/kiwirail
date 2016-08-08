package tw

object model {
  type Distance = Int
  type Town = String
  case class Route(start: Town, end: Town)
}