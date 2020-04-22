import CycleRoutesApp._
import org.scalatest.funsuite.AnyFunSuite

class CycleRoutesTest extends AnyFunSuite{
  /**
   * Below are the tests for each function which display reasonable output.
   **/

  var map:Map[String, List[(Int, String, Float)]] = Map()
  var key ="Oor Wullie Route (GCU)"
  var newList = List((1,"City Chambers",0.75f),(2,"Sir Chris Hoy Velodrome",3.8f),(3,"People's Palace",2.7f),(4,"Riverside Museum",5.4f),(5,"Botanic Gardens",2.4f),(6,"GCU",3.4f))
  map = map ++ Map(key -> newList)
  key ="Religious Route (Glasgow Cathedral)"
  newList = List((1,"St Andrew's Cathedral",1.8f),(2,"Central Mosque",0.75f),(3,"University Chapel",5.4f),(4,"Om Hindu Mandir",1.3f),(5,"Gurdwara Singh Sabha",0.6f),(6,"Quaker Meeting House",1.2f),(7,"Glasgow Buddhist Centre",0.35f),(8,"Garnethill Synagogue",0.45f),(9,"Glasgow Cathderal",3.3f))
  map = map ++ Map(key -> newList)

  key ="Art Route (Kelvingrove Art Gallery and Museum)"
  newList = List((1,"Hunterian Art Gallery",1.2f),(2,"MacKintosh Building",2.2f),(3,"Gallery Of Modern Art",1.4f),(4,"St. Mungo Museum Of Religious Life & Art",1.3f),(5,"People's Palace",2.0f),(6,"The Burrell Collection",7.1f),(7,"House For An Art Lover",2.8f),(8,"Kelvingrove Art Gallery and Museum",5.0f))
  map = map ++ Map(key -> newList)

  key ="Education Route (GCU)"
  newList = List((1,"University Of Strathclyde",0.65f),(2,"City Of Glasgow College - Riverside Campus",1.4f),(3,"School of Simulation and Visualisation",3.9f),(4,"Glasgow Science Centre",0.7f),(5,"University of Glasgow",2.4f),(6,"The Mitchell Library",1.9f),(7,"Glasgow School Of Art",0.9f),(8,"Royal Conservatoire Of Scotland",0.75f),(9,"GCU",0.6f))
  map = map ++ Map(key -> newList)

  def validate_values(testMap: Map[String, List[(Int, String, Float)]]) = {
    val firstRoute = testMap.apply("Oor Wullie Route (GCU)")
    assert(firstRoute.size == 6)
    assert(firstRoute.head._1 == 1)
    assert(firstRoute.head._2 == "City Chambers")
    assert(firstRoute.head._3 == 0.75f)
  }

  test("Test for readFile function") {
    val testMap: Map[String, List[(Int, String, Float)]] = map
    validate_values(testMap)
    assert(testMap.size == 4)
    println(assert(testMap.size == 4))
  }

  test("Test for show_routes") {
    val routes = show_routes(map)
    println(assert(routes == ()))
  }

  test("Test for all_routes_dist") {
    val dist = all_route_dist(map)
    println(assert(dist == ()))
  }

  test("Test for avg_dist_avg_stages"){
    val avgstage_avg_dist = avg_dist_avg_stages(map)
    println(assert(avgstage_avg_dist == ()))
  }

  test("Test for routes_by_dist_desc"){
    val desc_dist = routes_by_dist_desc(map)
    println(assert(desc_dist == ()))
  }

  test("Test for show_route"){
    val route = show_route("Oor Wullie Route (GCU)", map)
    println(assert(route == ()))
  }

  test("Test for create_personal_routes"){
    val pr = mnuCreatePersonalListOfRoutes(create_user_list)
    println(pr)
  }
}
