
trait Expression
case class BoundVariable(id:Int) extends Expression
case class Number(n:Int) extends Expression  
case class FunctionCall(f:Function, args:List[Expression]) extends Expression

trait Function {
  def arity : Int
}
trait Binary extends Function {
  override def arity = 2
}
case class Add() extends Binary
case class Multiply() extends Binary
case class Subtract() extends Binary
case class Divide() extends Binary
case class UserDefined(val body:Expression, val num_args:Int) extends Function {
  override def arity = num_args
}


object terms {

  // want to tighten to: Closed -> Number
  def evaluate(expr:Expression) : Number = expr match {
    case m @ Number(n) => m
    case FunctionCall(f, args) => apply(f, args map evaluate)
    case BoundVariable(id) => throw new RuntimeException("cant eval bound var")
  }


  // want to tighten the type to Expression -> List[Number] -> Closed
  def substitute(expr:Expression, subs:List[Number]) : Expression = expr match {
    case m @ Number(n) => m
    case FunctionCall(f, args) => FunctionCall(f, args.map(substitute(_, subs)))
    case BoundVariable(id) => subs(id)
  }

  // tight
  def apply(f:Function, args:List[Number]) : Number = {
    if (f.arity != args.size)
       throw new RuntimeException("Wrong number arguments")
     
    f match { 
      case UserDefined(body, _) =>  evaluate(substitute(body, args))
      case _ =>  args match {
                        case Number(n)::Number(m)::Nil => f match {
                              case Add() => Number(n + m)
                              case Multiply() => Number(n * m)
                              case Subtract() => Number(n - m)
                              case Divide() => Number(n / m)
                        }
                        case _ => throw new RuntimeException("no function can evaluate this")

      }
    }
  }
  def main(args: Array[String]) {

    val n = Number(4)
    val m = Number(5)
    val e1 = FunctionCall(Add(), List(n,m)) 
    
    val mult2 = UserDefined(FunctionCall(Add(), List(BoundVariable(0), BoundVariable(0))), 1)
    println(evaluate(FunctionCall(mult2, List(FunctionCall(mult2, List(Number(3)))))))
    println(evaluate(e1))
    println(evaluate(FunctionCall(mult2, List(Number(3)))))
  }
}

/*
import org.scalatest._

object TermTest extends FunSuite {

  test("cheese") {
    val mult2 = UserDefined(FunctionCall(Add(), List(BoundVariable(0), BoundVariable(0))), 1)
    val fourAddFive = FunctionCall(Add(), List(Number(4), Number(5))) 
    val threeTimesFour = FunctionCall(mult2, List(FunctionCall(mult2, List(Number(3)))))
    assert(Number(12) == terms.evaluate(threeTimesFour)) 
    ///assert(Number(9) == terms.evaluate(fourAddFive))
    //assert(Number(6) == terms.evaluate(FunctionCall(mult2, List(Number(3)))))
  }
}
*/

