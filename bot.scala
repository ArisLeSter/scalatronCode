import scala.util.Random

// Manage the response to the server
class ControlFunction {
  def respond(input: String) = {
    val (opcode, paramMap) = CommandParser(input)
    if(opcode=="React") {
      Bot(paramMap("view")).Decision().applyStrategy("foodStrategy")
    } else {
      ""
    }
  }
}


case class Bot(env : String){

  val botPosition = Sight().center

  // Sight managing class
  case class Sight() {
    val len = env.length()
    val size = math.sqrt(len).toInt
    val center = (size/2, size/2)

    def isInSight(obj : String) = env contains obj

    def indexToCoord(index : Int, size : Int): (Int, Int) = {
      (index % size, index / size)
    }

    def distance(xA : Int, yA : Int, xB : Int, yB : Int) = {
      math.sqrt(scala.math.pow(xB-xA, 2) + scala.math.pow(yB-yA, 2))
    }

    def detectClosest(c : Char): (Int, Int) ={
      val indexes = env
        //.view // lazy collection
        .zipWithIndex // zip element with their index
        .filter(_._1 == c) // filter tuple for specific char
        .map(elt => elt._2) // extract only index


      val closestEltCoords = indexes
        .map(index => indexToCoord(index, size)) // convert to coords
        .map(coords => (coords, distance(coords._1, coords._2, center._1, center._2))) // add distance
        .minBy(_._2) // find the minimum distance
        ._1 // return the coordinates

      closestEltCoords
    }

  }

  // Movement managing class
  case class Movement() {

    val x0 = botPosition._1
    val y0 = botPosition._2

    def goToCell(xPos: Int, yPos: Int): String = {

      var nextX = 0
      var nextY = 0

      if (xPos < x0){ nextX = -1 }
      if (xPos > x0){ nextX = 1 }

      if (yPos < y0){ nextY = -1 }
      if (yPos > y0){ nextY = 1 }

      s"Move(direction=$nextX:$nextY)"
    }

    def randomMove(): String = {

      val start = -1
      val end = 1
      val nextX = start + Random.nextInt( (end - start) + 1 )
      val nextY = start + Random.nextInt( (end - start) + 1 )
      s"Move(direction=$nextX:$nextY)"
    }

  }

  // Decision managing class
  case class Decision(){

    def applyStrategy(strategy : String): String ={
      strategy match {
        case "foodStrategy" => foodStrategy() + "|Status(text=food stategy)"
        case _ => "Status(text=No strategy)" // do nothing
      }
    }

    def foodStrategy(): String ={
      if( Sight().isInSight("p") ){ // If there is food in sight
        val closestFoodCoords = Sight().detectClosest('p') // detect the closest food
        Movement().goToCell(closestFoodCoords._1, closestFoodCoords._2) // go there
      }
      else { // If there is not food in sight
        Movement().randomMove() // perform a random move
      }
    }

  }
}

// Parse the input from the server
// input : "React(generation=0,time=100,view=__W_W_W__,energy=100)"
object CommandParser {
  def apply(command: String) = {
    def splitParam(param: String) = {
      val segments = param.split('=')
      if( segments.length != 2 )
        throw new IllegalStateException("invalid key/value pair: " + param)
      (segments(0),segments(1))
    }

    val segments = command.split('(')
    if( segments.length != 2 )
      throw new IllegalStateException("invalid command: " + command)

    val params = segments(1) // right part
      .dropRight(1) // discard ')'
      .split(',') // separate elements
    val keyValuePairs = params.map( splitParam ) // map params element by applying splitParam
      .toMap
    (segments(0), keyValuePairs)
  }
}

// Generates response for server
class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}

