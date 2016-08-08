import org.scalatest.FunSpec
import org.scalatest.Matchers
import tw.TrainScheduler
import tw.model.{Distance, Route}

class TrainSchedulerSpec extends FunSpec with Matchers {

  val kiwiland = Map[Route, Distance](
    Route("A","B") -> 5,
    Route("B","C") -> 4,
    Route("C","D") -> 8,
    Route("D","C") -> 8,
    Route("D","E") -> 6,
    Route("A","D") -> 5,
    Route("C","E") -> 2,
    Route("E","B") -> 3,
    Route("A","E") -> 7
)

  val trainScheduler = new TrainScheduler(kiwiland)

  describe("calculating distances") {
    it("should calculate distance of the route A B C") {
      trainScheduler.journeyLength(Seq("A", "B", "C")) shouldBe "9"
    }

    it("should calculate distance of the route A D") {
      trainScheduler.journeyLength(Seq("A", "D")) shouldBe "5"
    }

    it("should calculate distance of the route A D C") {
      trainScheduler.journeyLength(Seq("A", "D", "C")) shouldBe "13"
    }

    it("should calculate distance of the route A E B C D") {
      trainScheduler.journeyLength(Seq("A", "E", "B", "C", "D")) shouldBe "22"
    }

    it("should calculate distance of the route A E D") {
      trainScheduler.journeyLength(Seq("A", "E", "D")) shouldBe "NO SUCH ROUTE"
    }
  }



  /**	6.	The number of trips starting at C and ending at C with a maximum of 3 stops.
    In the sample data below, there are two such trips: C-D-C (2 stops). and C-E-B-C (3 stops).
    */
  describe("number of trips"){
    it("should count the number of routes from C to C with a max of 3 stops"){
    trainScheduler.numberOfTrips(Route("C", "C"), 3, Seq.empty) shouldEqual 4
    }
  }

}
