/**
 *
 *
 */
object Chapter4 extends App {
  abstract class Term
  case class TmTrue(info: Object) extends Term
  case class TmFalse(info: Object) extends Term
  case class TmIf(info: Object, t1:Term, t2:Term, t3:Term) extends Term
  case class TmZero(info: Object) extends Term
  case class TmSucc(info: Object, t:Term) extends Term
  case class TmPred(info: Object, t:Term) extends Term
  case class TmIsZero(info: Object, t:Term) extends Term

  def isnumericval(t:Term):Boolean = t match {
    case TmZero(_) => true
    case TmSucc(_, t1) => isnumericval(t1)
    case _ => false

  }

  def isval(t:Term):Boolean = t match {
    case TmTrue(_) => true
    case TmFalse(_) => true
    case _ => isnumericval(t)
  }

  val info = new Object

  def eval1(t:Term):Term = t match {
    case TmIf(_, TmTrue(_), t2, t3) => t2
    case TmIf(_, TmFalse(_), t2, t3) => t3
    case TmIf(fi, t1, t2, t3) =>
      TmIf(fi, eval1(t1), t2, t3)
    case TmSucc(_, t1) => eval1(t1)
    case TmPred(_, TmZero(_)) => TmZero(info)
    case TmPred(fi, TmSucc(_, t1)) =>
      if(isnumericval(t1))
        t1
      else
        TmPred(fi, eval1(t1))
    case TmPred(fi, t1) => TmPred(fi, eval1(t1))
    case TmIsZero(_, TmZero(_)) => TmTrue(info)
    case TmIsZero(fi, TmSucc(_, t1)) =>
      if(isnumericval(t1))
        TmFalse(info)
      else
        TmIsZero(fi, eval1(t1))
    case TmIsZero(f1, t1) => TmIsZero(f1, eval1(t1))
    case _ => throw new Exception("NoRuleApplies")
  }

  def eval(t:Term):Term = try {
    eval(eval1(t))
  } catch {
    case _: Throwable => t
  }

  def stringfy(t:Term):String = t match {
    case TmTrue(_) => "true"
    case TmFalse(_) => "false"
    case TmZero(_) => "zero"
    case TmSucc(_, t1) => "succ(" + stringfy(t1) + ")"
    case TmPred(_, t1) => "pred(" + stringfy(t1) + ")"
    case TmIsZero(_, t1) => "iszero(" + stringfy(t1) + ")"
    case TmIf(_, t1, t2, t3) => "if (" + stringfy(t1) + ") then (" + stringfy(t2) + ") else (" + stringfy(t3) + ")"
    case _ => ""
  }

  def evalPrint(t:Term) {
    println(stringfy(t))
    println(" => " + stringfy(eval(t)))
  }

  def testRun() {
    val zero = TmZero(info)
    val tru = TmTrue(info)
    val fls = TmFalse(info)
    val one = TmSucc(info, TmZero(info))

    evalPrint(tru)
    evalPrint(fls)
    evalPrint(zero)
    evalPrint(one)
    evalPrint(TmIf(info, tru, zero, one))
    evalPrint(TmIf(info, fls, zero, one))
    evalPrint(TmIf(info, TmIsZero(info, one), zero, one))
    evalPrint(TmPred(info, TmSucc(info, zero)))
    evalPrint(TmIf(info, TmIsZero(info, TmPred(info, TmSucc(info, zero))), tru, fls))
  }

  import scala.util.parsing.combinator._

  class ArithmeticExpressionParser extends RegexParsers {
    def value : Parser[Term] = (
          "true" ^^ (_ => TmTrue(info))
        | "false" ^^ (_ => TmFalse(info))
      )
    def ifExpr : Parser[Term] = ( "if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ {
      case t1 ~ t2 ~ t3 => TmIf(info, t1, t2, t3)
    }
    def term : Parser[Term] = value | ifExpr
  }

  object ParseExpr extends ArithmeticExpressionParser {
    def showPrompt() {
      print("chap4> ")
    }
    def run() {
      showPrompt()
      for( ln <- io.Source.stdin.getLines() ) {
        val res = parseAll(term, ln)
        res match {
          case ParseExpr.Success(r, n) => evalPrint(r)
          case ParseExpr.Failure(msg, n) => println(msg)
          case ParseExpr.Error(msg, n) => println(msg)
        }
        showPrompt()
      }
    }
  }

  ParseExpr.run()
}
