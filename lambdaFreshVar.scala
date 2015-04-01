import scala.collection.mutable.ListBuffer
import scala.util.Random

trait Exp
case class App(f:Exp, e:Exp) extends Exp
case class Abs(v:Var, e:Exp) extends Exp
case class Var(name:String, id:Int) extends Exp

object lambdaFreshVar {


  var debug = true

  def log(msg:String) = {
    if(debug)
      println(msg)
  }
  def logev(msg:String, e:Exp) = log(msg + pp(e))

  def stuck(e:Exp) : Boolean = {
    e match {
      case v @ Var(_,_) => true
      case Abs(v, f) => stuck(f)
      case App(f, g) => !f.isInstanceOf[Abs] && stuck(f) && stuck(g)
    }
  }

  def evaluate(e:Exp) : Exp = {
    if(!stuck(e))
      evaluate(leftapply(e))
    else
      e
  }

  def leftapply(x:Exp) : Exp = {
      x match {
        case v @ Var(_,_) => v
        case Abs(v,e) => Abs(v, leftapply(e))
        case App(Abs(v, e), g) => substitute(g,v,e)
        case App(f, g) => if(!stuck(f))
                             App(leftapply(f), g)
                          else
                             App(f, leftapply(g))
     }
  }

  def evaluate(e:Exp) : Exp = {
    if(!stuck(e))
      evaluate(leftapply(e))
    else
      e
  }

  // substitute g for v in e
  def substitute(g:Exp, v:Var, e:Exp) : Exp = {
    log("sub " + pp(g) + " for " + pp(v) + " in " + pp(e))
    e match {
      case App(h, k) => App(substitute(g, v, h), substitute(g, v, k)) 
      case w @ Var(_,_) => if(w == v) g else w
      case abs @ Abs(w, f) => if (w == v) { // v is shadowed 
                                abs
                              }
                              else if (freeVars(g).contains(w)) {
                                substitute(g, v, alpha(abs, g)) // alpha convert Abs(w, f) to Abs(w', f) so that w' is not in freeVars(g)
                              }
                              else {
                                Abs(w, substitute(g, v, f))
                              }
    }
  }

  // alpha convert a so that w' is not in freeVars of g
  def alpha(a:Abs, g: Exp) : Abs = {
    log("alpha conv " + pp(a) + " where var is not free in " + pp(g))
    a match {
      case Abs(w, f)  => { 
                            val wgid = minSimNotIn(w, g).id 
                            val wfid = minSimNotIn(w, f).id
                            val w_ = Var(w.name, if(wgid < wfid) wfid else wgid) 
                            Abs(w_, substitute(w_, w, f))
                         }
    }
  }

  def minSimNotIn(w:Var, g:Exp) : Var = w match {
    case Var(name, id) => {
                            val nextVar = Var(name, id + 1)
                            if(freeVars(g).contains(nextVar))
                              minSimNotIn(nextVar, g)
                            else 
                              nextVar
    }
  }

  def freeVars(e:Exp) : Set[Var] = e match {
    case v @ Var(_,_) => Set(v)
    case Abs(v, f) => freeVars(f) - v
    case  App(f, g)  => freeVars(f) ++ freeVars(g)
  }

  def pp(e:Exp) : String = e match {
    case Var(name, id) => if(id == 0) name else name + id.toString
    case Abs(v, f) => "(\\" + pp(v) + "." + pp(f) + ")"
    case App(f, g) => "(" + pp(f) + " " + pp(g) + ")"
  }

  def ev(exp: Exp) = println(pp(evaluate(exp))) 

  def nat(n:Int) : Exp = {
    val x = Var("x", 0)
    val f = Var("f", 0)
    def applications(k:Int) : Exp = if(k == 0) x else App(f, applications(k - 1))
    Abs(f, Abs(x, applications(n)))
  }



  def main(args: Array[String]) {
    val y = Var("y", 0)
    val z = Var("z", 0)
    val f = Var("f", 0)
    val x = Var("x", 0)
    val m = Var("m", 0)
    val n = Var("n", 0)
    val g = Var("g", 0)
    val h = Var("h", 0)
    val u = Var("u", 0)
    val un = Var("un", 0)
    val T = Abs(x, Abs(y, x))
    val F = Abs(x, Abs(y, y))
    val ifElseThen = Abs(x, Abs(y, Abs(z, App(App(x, y), z))))
    val zp = Abs(m, App(App(m, App(T, F)), T))
    val succ = Abs(m, Abs(f, Abs(x, App(f, App( App(m, f), x)))))
    val plus = Abs(m, Abs(n, Abs(f, Abs(x, App(App(m, f), App(App(n, f), x))))))
    val pred = Abs(n, Abs(f, Abs(x, App(App(App(n, Abs(g, Abs(h, App(h, App(g, f))))), Abs(u, x)), Abs(u, u)))))
    val times = Abs(m, Abs(n, Abs(f, App(m, App(n, f)))))
    val yComb = Abs(f, App(Abs(x, App(f, Abs(y, App(App(x, x), y)))), Abs(x, App(f, Abs(y, App(App(x, x), y))))))
    val timesRec = Abs(f, Abs(m, Abs(n, App(App(App(ifElseThen, App(zp, m)), n), App(App(plus, n), App(App(f, App(pred, m)), n))))))
    val times2 = App(yComb, timesRec)
    val fact = Abs(f, Abs(n, App(App(App(ifElseThen, App(zp, n)), nat(1)), App(App(times, n), App(f, App(pred, n))))))
    val omega = App(Abs(x, App(x, x)), Abs(x, App(x, x)))



val constant3app = Abs(z, Abs(z, Abs(z, Abs(z, nat(0)))))

val rec = Abs(f, Abs(n, App(App(App(zp, n), nat(1)), App(f, App(n, g)))))
val make = Abs(f, Abs(n, App(App(App(ifElseThen, App(zp, n)), m), App(m, App(f, App(pred, n))))))
val make2 = Abs(f, Abs(n, App(App(App(zp, n), nat(0)), App(succ, App(f, App(pred, n))))))
val branch = Abs(n, App(App(App(zp, n), nat(0)), nat(2)))


  debug = false


  def ps(e:Exp) = println(pp(e) + " " + stuck(e))

  val tests = List(App(App(z, App(Abs(x,x), y)), u),
                  App(App(f, g), App(Abs(x,x), y)),
                  App(z, u),
                  App(x,App(z,y)),
                  x,
                  Abs(x,x),
                  App(App(z, App(Abs(x,x), y)), u),
                  App(App(f, g), App(Abs(x,x), y)),
                  App(App(App(App(yComb, rec), constant3app), f), x),
                  App(App(App(App(yComb, fact), nat(4)), f), x),
                  App(App(App(App(yComb, fact), nat(5)), f), x))
  for(t <- tests) {
    ps(evaluate(t))
  }




/*    
  //ev(App(App(App(App(yComb, make2), n), f), x))
  //ev(App(App(App(App(yComb, make), nat(0)), f), x))
  //println(pp(evaluate(App(App(App(App(yComb, fact), zero), f), x))))
  // println(pp(evaluate(App(App(App(App(yComb, fact), one), f), x))))
  // println(pp(evaluate(App(App(App(App(yComb, fact), two), f), x))))
  // println(pp(evaluate(App(App(App(App(yComb, fact), three), f), x))))
  //println(pp(evaluate(App(App(App(App(yComb, fact), four), f), x))))
  println(pp(evaluate(App(App(App(succ, four), f), x))))
      println(pp(evaluate(App(App(App(App(times, zero), four), f), x))))
    println(pp(evaluate(App(App(App(App(times, one), four), f), x))))
    println(pp(evaluate(App(App(App(App(times, two), four), f), x))))
    println(pp(evaluate(App(App(App(App(times, three), four), f), x))))
     val captor = Abs(x, Abs(y, x))
     val captive = Abs(z, y)
     println(pp(evaluate(App(captor, captive))))
  println(freeVars(App(x, Abs(y, z))))
  println(freeVars(succ))
  println(freeVars(pred))
  println(alpha(T, un))
  println(alpha(F, un))
   
*/ 

   }

  
      




}
                            
  




