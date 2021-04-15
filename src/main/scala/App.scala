import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap

/**
 *  GLASGOW CALEDONIAN UNIVERSITY
 *  This application was created by Jonathan Yaniv Ben Avraham
 *  Student ID: S17*****
 *  For Module: 3I322912
 *  ADVANCED PROGRAMMING
 *  Instructor: Martin Gallacher
 *  04/2020
 */

object CycleRoutesApp extends App {
  /**
   * APPLICATION is intended to display cycle routes according to options
   * chosen by a user given a menu of options.
   *
   * The application also supports the user in creating a list of personalised preferred routes.
   */

  // read the routes from file
  var file_name = "src/main/cycleroutes.txt"
  val mapdata = readFile(file_name)

  /**
   *  Application Utility Functions
   */
  // reads data from an input file and returns a map containing a name and a map containing place and float
  def readFile(filename:String):Map[String, List[(Int,String,Float)]] ={
    // A buffer is added to prevent nothing being returned to the terminal
    var mapBuffer : Map[String, List[(Int, String, Float)]] = Map()

    // a try catch is used for exception handling, should the file be unreadable, an error will be displayed to
    try{
      // A for loop is initiated to allow me to parse through the file line by line
      for (line <- Source.fromFile(filename).getLines()) {
        var splitline = line.split(",").map(_.trim).toList     // split line at , and convert to List

        // The head of the list is isolated and stored in the key value.
        // This key value stores the name of the cycle route
        val key = splitline.head
        // The rest of the list is then applied to splitLine
        splitline = splitline.tail

        // an empty list accepting a tuple of int, string, and float is created.
        var value_list : List[(Int, String, Float)] = List()

        // A while loop checking that the value of splitline is not empty
        while (splitline.nonEmpty){
          // assigning the rest of splitline to the tuple
          val tuple = splitline.head.split(":") match {
            // in the case where this matches an array with three values, it is to convert those values to Int, String and Float
            case Array(int,str,flo) => (int.toInt, str.toString, flo.toFloat)
          }
          // I then concatenate the values into the empty list I created on line 47
          value_list = value_list :+ tuple
          // the rest of the file is then assigned to splitline for the loop to continue
          splitline = splitline.tail
        }

        // I now add the keys and values to the buffer
        mapBuffer = mapBuffer ++ Map(key -> value_list)
      }
      println(
        """Welcome to the CycleRoutes App :)
          |
          |""".stripMargin)
    } catch {
      case ex : Exception => println("Sorry an exception has been caught, \nit appears the file is not parse-able")
    }
    mapBuffer
  }

  /**
   * Application Menu Functions and Option Handling
   */

  // an action map is created to allow for option handling
  val actionMap = Map[Int, () => Boolean](1 -> handleOne, 2 -> handleTwo, 3 -> handleThree, 4 -> handleFour,
    5 -> handleFive, 6 -> handleSix, 7 -> handleSeven)

  // option is set to 0 automatically, assigning it to Integer Type.
  var opt = 0

  // a do-while loop is implemented to allow for looping over the menu options while the function returns true.
  do {
    try {
      opt = readOption // option is grabbed via the readOption method defined below
    }catch{
      case ex: Exception => println(
        """Sorry that is not a valid option
          |Please try again.
          |""".stripMargin)
    }
  } while (menu(opt)) // if the function returns false, the program will stop.


  // This function displays the menu to the user and reads input
  def readOption:Int ={
    println(
      """|Please select an option from the following list
         |  1 - Show me all route values
         |  2 - Show me the total distance for each route
         |  3 - Show me the average distance and average number of stages for all routes
         |  4 - Report of all routes in desc. order by total distance
         |  5 - Show me a specific route
         |  6 - Create a personalised list of routes
         |  7 - Quit
         |""".stripMargin
    )

    readInt()
  }

  // invokes selected menu option
  // finds corresponding function to invoke in action map using get
  // pattern matching used as get returns an Option
  def menu(option:Int):Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that option is not recognised. ")
        true
    }
  }

  def handleOne():Boolean ={
    mnuShowAllRouteValues(show_routes)
    true
  }

  def handleTwo():Boolean = {
    mnuShowTotalDistForAllRoutes(all_route_dist)
    true
  }

  def handleThree():Boolean = {
    mnuShowAvgDistAvgStages(avg_dist_avg_stages)
    true
  }

  def handleFour():Boolean = {
    mnuReportRoutesDescByDist(routes_by_dist_desc)
    true
  }

  def handleFive():Boolean = {
    try{
      mnuShowSpecificRoute(show_route)
    } catch {
      case ex: Exception => println("Sorry but you did not enter a valid route name.")
    }

    true
  }

  def handleSix():Boolean = {
    mnuCreatePersonalListOfRoutes(create_user_list)
    true
  }

  def handleSeven():Boolean ={
    println("Thank you for using the CycleRoutes App :)")
    false
  }

  /**
   * Functions that invoke and interact with the user
   */

  // This function passes mapdata as a variable data, to show_routes
  // It is called by handleOne()
  def mnuShowAllRouteValues(f: (Map[String, List[(Int, String, Float)]]) => Unit) = {
    var data = mapdata
    f(data)
  }

  // This function passes mapdata to all_route_dist
  // It is called by handleTwo()
  def mnuShowTotalDistForAllRoutes(f:( Map[String, List[(Int, String, Float)]]) => Unit) = {
    f(mapdata)
  }

  // This function passes mapdata to avg_dist_avg_stages
  // It is called by handleThree()
  def mnuShowAvgDistAvgStages(f: (Map[String, List[(Int, String, Float)]]) => Unit) = {
    f(mapdata)
  }

  // This function passes mapdata to routes_by_dist_desc
  // It is called by handleFour()
  def mnuReportRoutesDescByDist(f: (Map[String, List[(Int, String, Float)]])=> Unit) = {
    f(mapdata)
  }

  // This function accepts a user input, assigns it to a value of route and passes it to showRoute
  // It is called by handleFive()
  def mnuShowSpecificRoute(f: (String, Map[String, List[(Int, String, Float)]]) => Unit) = {
    val route = readLine("Enter Route: ") // Takes user input and applies it to the value of team
    f(route, mapdata)
  }

  def mnuCreatePersonalListOfRoutes(f: (List[String], Map[String, List[(Int, String, Float)]]) => Map[String, String]) = {
    var routeList: List[String] = List()
    var count = 0
    var route = ""
    do {
      route = readLine("Enter Route: ")
      count + 1
      routeList = routeList.appended(route)
      routeList
    }while (route != "")
    var userMap = f(routeList, mapdata)
    println(
      s"""Your personal route has been created
         |$userMap
         |""".stripMargin)

  }

  /**
   * OPERATING FUNCTIONS
   */

  // This function formats and displays all items in the map
  def show_routes(map:Map[String, List[(Int, String, Float)]]) ={
    var routename = ""
    var place_id = 0
    var place_name = ""
    var output = """"""

    for ( x <- map){
      routename = x._1
      output = s"""Route name: $routename"""
      for (y <- x._2){
        place_id = y._1
        place_name = y._2
        var place_dist = y._3
        output = output +
          s"""
             |place id: $place_id
             |place name: $place_name
             |place distance: $place_dist
             |""".stripMargin
        output
      }
      println(output)
    }
  }

  // This function displays the total distance for each route
  def all_route_dist(map:Map[String, List[(Int, String, Float)]]) ={
    var routename = ""
    var routedistTotal = 0.0
    var routepoint_dist = 0.0
    for (x <- map){
      routename = x._1
      var count = 0
      for ( y <- x._2){
        routepoint_dist = y._3
        count = count + 1
      }

      routedistTotal +=  routepoint_dist
      routename
      count
      routedistTotal
      println(
        s"""Route Name: $routename
           |has $count stages
           |and has a total distance of $routedistTotal km
           |""".stripMargin)
    }
  }


  // This function displays average number of stages and aveage distance over all routes
  def avg_dist_avg_stages(map:Map[String, List[(Int, String, Float)]]) ={

    var average_stage:Int = 0
    var average_dist:Float = 0.00f

    var list:List[(Int, Float)] = List()
    list = List(averages_for_routes(map))

    for (l <- list){
      average_dist = l._2
      average_stage = l._1
    }

    println(
      s"""Average number of stages over all routes: $average_stage
         |Average distance over all of the routes: $average_dist km
         |""".stripMargin)
  }

  // This function displays all routes by descending distance
  def routes_by_dist_desc(map:Map[String, List[(Int, String, Float)]]) = {
    var mapDesc: Map[String, Float] = Map.empty[String, Float]
    var route_total_dist:Float = 0.00f
    for (x <- map){
      var routename = x._1
      route_total_dist = 0.00f
      for (y<- x._2){
        var dist = y._3
        route_total_dist += dist
      }
      routename
      route_total_dist
      mapDesc = mapDesc + (routename -> route_total_dist)
      mapDesc
    }


    var listmap:ListMap[String,Float] = ListMap(mapDesc.toSeq.sortWith(_._2 > _._2):_*)
    listmap

    for ( x <- listmap){
      var routename = x._1
      var dist = x._2
      println(
        s"""Route: $routename
           |distance: $dist km
           |""".stripMargin)
    }
  }

  // This function is called by mnuShowSpecificRoute and outputs a specific route through the routeName parameter
  def show_route(routeName: String, map:Map[String, List[(Int, String, Float)]]) = {

    var stage_name:String = ""
    var stage_dist:Float = 0.00f

    for ( x <- map) {
      if (x._1 == routeName){
        println(
          s"""Routename: $routeName
             |Stages:
             |""".stripMargin)
        for ( y <- x._2){
          stage_name = y._2
          stage_dist = y._3
          println(
            s"""  - name: $stage_name
               |  - distance: $stage_dist km
               |""".stripMargin)
        }
      }
    }
  }

  // This function creates a unique map for users which contains route names and comments for each route.
  def create_user_list(lst:List[String], map:Map[String, List[(Int, String, Float)]]):Map[String, String] = {
    var userMap:Map[String, String] = Map.empty[String, String]
    var key:String = ""
    var key1:String = ""
    var comment:String = ""

    for (x <- lst){
      if (map.contains(x)){
        key = x
        comment = readLine("Please enter a comment for your chosen route: ")
        userMap = userMap ++ Map(key -> comment)
      }
      userMap
    }
    userMap
  }


  /**
   * MATHEMATICAL FUNCTIONS
   */

  def averages_for_routes(map:Map[String, List[(Int, String, Float)]]) = {
    val (sum_of_stages, sum_of_distances) = sum_stages_sum_distance(map)
      .reduceLeft((x,y) => (x._1 + y._1, x._2 + y._2))

    val (average_distance, average_stages) = (sum_of_distances.toFloat / map.size, sum_of_stages / map.size)
    (average_stages, average_distance)
  }

  def sum_stages_sum_distance: PartialFunction[Map[String, List[(Int, String, Float)]], List[(Int, Float)]] = {
    case routes => for (route <- routes.toList) yield {
      val (stage_count, total_dist) = stage_count_total_dist(route._2)
      (stage_count, total_dist)
    }
  }

  def stage_count_total_dist: PartialFunction[List[(Int, String, Float)], (Int, Float)] ={
    case stages => stages.foldLeft((0, 0.00f))((tuple, stage) => {
      (tuple._1 + 1, tuple._2 + stage._3)
    })
  }


  def sum(list:List[Double]):Double = list match{
    case Nil => 0
    case x :: tail => x + sum(tail)
  }

  def calc_average(list:List[Double]):Double ={
    list.foldLeft(0)((x, y) => ((sum(list).toInt / mapdata.size)))
  }

  def length(ls:List[Double]):Double ={
    def listLengthTR_nested(ls:List[Double], len:Int):Double = ls match {
      case Nil => len
      case h :: tail => listLengthTR_nested(ls.tail, len+1)
    }
    listLengthTR_nested(ls, 0)
  }



}
