// Main Part 4 about finding Knight's tours
//==========================================


object M4a {

// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below. Also have a look whether the functions
// at the end of the file are of any help.

import scala.annotation.tailrec

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1) Complete the function that tests whether the position x
//    is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  val isWithinBoard = (x._1 < dim && x._1 >= 0) && (x._2 < dim && x._2 >= 0) 
  val isNotOnPath = !(path.contains(x)) 
  isWithinBoard && isNotOnPath  
}

//(2) Complete the function that calculates for a position x
//    all legal onward moves that are not already in the path. 
//    The moves should be ordered in a "clockwise" manner.
 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {

  //change order to make clockwise
  //add x and y to all possible options that can be made starting from (0,0)
  val possibleMoves = List((1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)).map(pos =>(pos._1+x._1,pos._2+x._2))

  //filter out illegal moves
  possibleMoves.filter(is_legal(dim, path, _))

}


//some testcases
//
// assert(legal_moves(8, Nil, (2,2)) == 
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
// assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
// assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
// assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(3) Complete the two recursive functions below. 
//    They exhaustively search for knight's tours starting from the 
//    given path. The first function counts all possible tours, 
//    and the second collects all tours in a list of paths.

def count_tours(dim: Int, path: Path) : Int =  path match {

  //if path length becomes greater than the area of the board, we know it covered it
  case path if path.size >= dim * dim => 1
  case _ =>{

            //recursively map tours and add next legal moves to paths
            val mappedTours =  legal_moves(dim, path, path(0)).map(legalMove => count_tours(dim, legalMove :: path))
            //sum all the 1s
            mappedTours.sum
            }
  
}


def enum_tours(dim: Int, path: Path) : List[Path] = path match{
  
  case path if path.size >= dim*dim => List(path)
  case _ => {
    val legalMoves = legal_moves(dim, path, path(0)).map(legalMove => enum_tours(dim, legalMove :: path))
    legalMoves.flatten
  }

}

//(4) Implement a first-function that finds the first 
//    element, say x, in the list xs where f is not None. 
//    In that case Return f(x), otherwise None. If possible,
//    calculate f(x) only once.

@tailrec
def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match {
  
  case Nil => None
  case x::xs => {
    val value = f(x)
    if (value.isDefined){ 
          value
        } 
    else first(xs,f)
  }
}

// testcases
//
// def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None

// first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo)   // Some(List((4,0)))
// first(List((1, 0),(2, 0),(3, 0)), foo)          // None


//(5) Implement a function that uses the first-function from (4) for
//    trying out onward moves, and searches recursively for a
//    knight tour on a dim * dim-board.

def first_tour(dim: Int, path: Path) : Option[Path] = path match {

  case path if path.size >= dim*dim => Some(path)
  case _ => {
            first(legal_moves(dim, path, path(0)), madeMoveTo => first_tour(dim, madeMoveTo :: path))
          }

}


//tests from github
// first_tour(6, List((0, 0)))
// first_tour(4, List((0, 0))) == None


/* Helper functions

// for measuring time
def time_needed[T](code: => T) : T = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
  result
}

// can be called for example with
//
    // time_needed(count_tours(dim, List((0, 0))))
    //  time_needed(count_tours(5, List((0, 0))))
    // enum_tours(5, List((0, 0)))
    // time_needed(first_tour(6, List((0, 0))))
//
// in order to print out the time that is needed for 
// running count_tours


// for printing a board
def print_board(dim: Int, path: Path): Unit = {
  println()
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
    }
    println()
  } 
}


*/

}
