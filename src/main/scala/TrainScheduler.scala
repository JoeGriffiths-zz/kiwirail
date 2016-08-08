package tw

import tw.model.{Town, Distance, Route}

import scala.annotation.tailrec


class TrainScheduler(graph: Map[Route, Distance]) {

  type Journey = Seq[Town]
  def numberOfTrips(route: Route, maxStops: Int, routes: Seq[Seq[Town]]): Int = {

    val hopsToExplore = for (hopsToExplore <- connectedNodes(route.start)) yield hopsToExplore

    for (hop <- hopsToExplore) yield connectedNodes(hop.start)
    hopsToExplore


  }

    def journeyLength(journey: Journey): String = {
      val distances = getRoutes(journey, Seq.empty[Option[Distance]])
      getRoutes(journey, Seq.empty[Option[Distance]]) match {
        case noSuchRoute if distances.contains(None) => "NO SUCH ROUTE"
        case _ => distances.foldLeft(0) { (accum, iter) => accum + iter.getOrElse(0) }.toString
      }
    }

  @tailrec
  private def getRoutes(journey: Journey, distances: Seq[Option[Distance]]): Seq[Option[Distance]] = {
    if (journey.size == 1)
      distances
    else {
      val journeySegment = Route(journey.head, journey(1))
      val rollingDistances = distances :+ graph.get(journeySegment)
      getRoutes(journey.takeRight(journey.size - 1), rollingDistances)
    }
  }

  private def connectedNodes(town: Town) = for (connection <- graph.keys if connection.start == town) yield connection
}
