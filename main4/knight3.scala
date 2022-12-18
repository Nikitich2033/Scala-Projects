// Finding a single tour on a "mega" board
//=========================================

object M4c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

import scala.annotation.tailrec

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

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.


def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = ???

}
