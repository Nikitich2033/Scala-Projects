// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object C3b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def is_op(op: String) : Boolean = ops.contains(op)

def precLA(op1: String, op2: String) : Boolean = {
  
    if((precs.get(op1)).isDefined && (precs.get(op2)).isDefined ){
      precs(op1) >= precs(op2)
    }
    else false
	
}

def precRA(op1: String, op2: String) : Boolean = {
  if((precs.get(op1)).isDefined && (precs.get(op2)).isDefined ){
      precs(op1) > precs(op2)
    }
    else false
	
}


def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
	case x :: xs => {
	
		if (is_op(x)) {
      if(assoc(x) == RA){
        st match {
				case s :: ss if s == "(" => {
					syard(xs, x :: st, out)
				}
				case s :: ss if s == ")" => {
          val index = st.indexOf("(")
					syard(xs, st.drop(index + 1), out:::st.take(index))

				}
				case s :: ss if (precRA(s, x)) =>{
					syard(toks, ss, out ::: s :: Nil)
				}
				case _ => {
				
					syard(xs, x :: st, out)
				}
			}
      }
      else{
         st match {
				case s :: ss if s == "(" => {
					syard(xs, x :: st, out)
				}
				case s :: ss if s == ")" => {
				  val index = st.indexOf("(")
          syard(xs, st.drop(index + 1), out:::st.take(index))
				}
				case s :: ss if (precLA(s, x)) =>{
					syard(toks, ss, out ::: s :: Nil)
				}
				case _ => {
				
					syard(xs, x :: st, out)
				}
			}
      }
			
		}
  
		else if(x.forall(_.isDigit)){
			syard(xs,st, out ::: x :: Nil)
		}
		else if(x == "("){
			syard(xs, x :: st, out)
		}
		else if(x == ")"){
			val index = st.indexOf("(")
      syard(xs, st.drop(index + 1), out:::st.take(index))
		}
		else{
			
			println(x)
			out ::: x :: Nil
		}
		
	}
	case _ => {
		out ::: st
	}

}

// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +

// (4) Implement a compute function that produces an Int for an
// input list of tokens in postfix notation.

def compute(toks: Toks, st: List[Int] = Nil) : Int = {
  toks.foldLeft(List[Int]())(
        (list, token) => (list, token) match {
            case (x :: y :: zs, "*") => (y * x) :: zs
            case (x :: y :: zs, "+") => (y + x) :: zs
            case (x :: y :: zs, "-") => (y - x) :: zs
            case (x :: y :: zs, "/") => (y / x) :: zs
            case (x :: y :: zs, "^") => (BigInt(y).pow(x).toInt) :: zs
            case (_, _) => token.toInt :: list
        }).head

}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
