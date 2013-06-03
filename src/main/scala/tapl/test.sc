import java.util.NoSuchElementException



def nth[A](n:Int, xs:List[A]):A = (n, xs) match {
  case (0, x :: _ ) => x
  case (n, _ :: xs) => nth(n - 1, xs)
  case (n, Nil ) => throw new NoSuchElementException
}
/**
 *
 *
 */
abstract class Term
case class TmVar(info:Object, x:Int, n:Int) extends Term
case class TmAbs(info:Object, name:String, t:Term) extends Term
case class TmApp(info:Object, t1:Term, t2:Term) extends Term

class Binding
class Context(var ctx:List[(String, Binding)]) {
  def length():Int =
    ctx.length

  def index2name(info:Object, x:Int):String =
    nth(x, ctx)._1

  def pickFreshName(x:String):(Context, String) = {
    if ( ctx.exists(s => s._1 == x) )
      pickFreshName(x + "'")
    else
      (new Context((x, new Binding) :: ctx), x)
  }
}


def printTerm(ctx:Context, t:Term):Unit = t match {
  case TmAbs(fi, x, t1) =>
    val (ctx_, x_) = ctx.pickFreshName(x)
    print("(lambda " + x_ + ". ")
    printTerm(ctx_, t1)
    print(")")
  case TmApp(fi, t1, t2) =>
    print("(")
    printTerm(ctx, t1)
    print(" ")
    printTerm(ctx, t2)
    print(")")
  case TmVar(fi, x, n) =>
    if (ctx.length() == n)
      print(ctx.index2name(fi, x))
    else
      print("[bad index]")
}


def termShift(d:Int, t:Term):Term = {
  def walk(c:Int, tt:Term):Term = tt match {
    case TmVar(fi,x,n) =>
      if(x >= c)
        TmVar(fi, x + d, n + d)
      else
        TmVar(fi, x, n + d)
    case TmAbs(fi,x,t1) =>
      TmAbs(fi, x, walk(c + 1, t1))
    case TmApp(fi,t1,t2) =>
      TmApp(fi, walk(c, t1), walk(c, t2))
  }
  walk(0, t)
}

def termSubst(j:Int, s:Term, t:Term):Term = {
  def walk(c:Int, tt:Term):Term = tt match {
    case TmVar(fi,x,n) =>
      if(x == j+c)
        termShift(c, s)
      else
        TmVar(fi, x, n)
    case TmAbs(fi,x,t1) => TmAbs(fi, x, walk(c + 1, t1))
    case TmApp(fi,t1,t2) => TmApp(fi, walk(c, t1), walk(c, t2))
  }
  walk(0, t)
}

def termSubstTop(s:Term, t:Term):Term =
  termShift(-1, termSubst(0, termShift(1, s), t))

def isval(ctx:Context, t:Term):Boolean = t match {
  case TmAbs(_,_,_l) => true
  case _ => false
}


def eval1(ctx:Context, t:Term):Term = t match {
  case TmApp(fi, TmAbs(_,x,t12), v2) if (isval(ctx, v2)) =>
    termSubstTop(v2, t12)
  case TmApp(fi, v1, t2) if (isval(ctx, v1)) =>
    val t2_ = eval1(ctx, t2)
    TmApp(fi, v1, t2_)
  case TmApp(fi,t1,t2) =>
    val t1_ = eval1(ctx, t1)
    TmApp(fi,t1_,t2)
  case _ => throw new Exception()
}


def eval(ctx:Context, t:Term):Term = try {
  eval(ctx, eval1(ctx, t))
} catch {
  case e: Throwable => t
}


val info = new Object
val ctx = new Context(Nil)

// id = \x -> x
val id = TmAbs(info, "x", TmVar(info, 0, 1))


print("id = ")
printTerm(ctx, id)
println
// tru = \t -> \f -> t
val tru = TmAbs(info, "t", TmAbs(info, "f", TmVar(info, 0, 2)))


print("tru = ")
printTerm(ctx, tru)
println
// fls = \t -> \f -> f
val fls = TmAbs(info, "t", TmAbs(info, "f", TmVar(info, 1, 2)))


print("fls = ")
printTerm(ctx, fls)
println
// (id id)
print("(id id) = ")
printTerm(ctx, TmApp(info, id, id))

println
// \x. \x. x
print("\\x. \\x. x = ")
printTerm(ctx, TmAbs(info, "x", TmAbs(info, "x", TmVar(info, 0, 2))))

println
// (id id)
print("(id tru) = ")
printTerm(ctx, eval(ctx, TmApp(info, id, tru)))

println