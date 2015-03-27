
/*
trait PrimativeFunction
case class Add(arg1: Term, arg2:Term) extends PrimativeFunction
case class Multiply(arg1: Term, arg2:Term) extends PrimativeFunction
case class Subtract(arg1: Term, arg2:Term) extends PrimativeFunction
case class Divide(arg1: Term, arg2:Term) extends PrimativeFunction
case class Exponentiate(arg1: Term, arg2:Term) extends PrimativeFunction
*/


trait Expression
case class Number(n:Int) extends Expression
case class BoundVariable(id:Int) extends Expression
case class FunctionCall(f:Function, args:List[Expression]) extends Expression


trait Function extends Expression
case class Add() extends Function
case class UserDefined(num_args:Int, body:Expression) extends Function


object terms {
  def evaluate(expr:Expression) : Expression = expr match {
    case Number(n) => Number(n)
    case BoundVariable(id) => BoundVariable(id)
    case FunctionCall(f, args) => apply(f, args.map(evaluate))
  }

  def apply(f:Function, args:List[Expression]) : Expression = f match {
    case Add() => {
                  if (args.size != 2) 
                    throw new RuntimeException("wrong num args")
                  else args match {
                        case Number(n)::Number(m)::Nil => Number(n + m)
                        case _ => FunctionCall(f, args)
                      }
                }
    case UserDefined(num_args, body) => evaluate(substitute(body, args))
  }
      
  def substitute(expr:Expression, args:List[Expression]) : Expression = expr match {
    case Number(n) => Number(n)
    case BoundVariable(id) => args(id)
    case FunctionCall(f, callArgs) => FunctionCall(f, callArgs.map(a => substitute(a, args)))
  }

  def main(args: Array[String]) {
    val n = Number(4)
    val m = Number(5)
    val e1 = FunctionCall(Add(), List(n,m)) 
    
    val mult2 = UserDefined(1, FunctionCall(Add(), List(BoundVariable(0), BoundVariable(0))))


    println(evaluate(FunctionCall(mult2, List(FunctionCall(mult2, List(Number(3)))))))
    

    println(evaluate(e1))
    println(evaluate(FunctionCall(mult2, List(Number(3)))))




  }



}
  
