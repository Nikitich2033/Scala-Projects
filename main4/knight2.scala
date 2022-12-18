// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object M4b {

import scala.annotation.tailrec

// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  val isWithinBoard = (x._1 < dim && x._1 >= 0) && (x._2 < dim && x._2 >= 0) 
  val isNotOnPath = !(path.contains(x)) 
  isWithinBoard && isNotOnPath  
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {

  //change order to make clockwise
  //add x and y to all possible options that can be made starting from (0,0)
  val possibleMoves = List((1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)).map(pos =>(pos._1+x._1,pos._2+x._2))

  //filter out illegal moves
  possibleMoves.filter(is_legal(dim, path, _))

}

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

//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {

  legal_moves(dim,path,x).sortWith(legal_moves(dim,path,_).size < legal_moves(dim,path,_).size)

}

// github tests
// ordered_moves(8, List((3,4), (3,2)), (1,3)) == List((0,1), (0,5), (2,1), (2,5))
// ordered_moves(8, List((4,0)), (0,0)) == List((2,1), (1,2))
// ordered_moves(8, List((0,4)), (0,0)) == List((1,2), (2,1))

//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {

  if(path.size == dim * dim && legal_moves(dim, Nil, path.head ).contains(path(path.size-1))){
    Some(path)
  }
  else{
    first(ordered_moves(dim,path,path.head), x => first_closed_tour_heuristics(dim, x :: path))
  }
  
}

//github tests
// val test1 = first_closed_tour_heuristics(6, List((3,3)))
// val test2 = first_closed_tour_heuristics(6, List((3,3)))
// test1 == test2
// first_closed_tour_heuristics(6, List((3,3)))

//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = ???

//github tests
// first_tour_heuristics(8, List((0,0)))
// first_tour_heuristics(30, List((0,0))) 

}
