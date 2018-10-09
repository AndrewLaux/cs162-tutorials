
package edu.ucsb.cs.cs162.tuts.calculator

// A mathematical expression.
sealed trait Expr

// A variable expression with a name.
final case class Var(name: String) extends Expr

// A number expression with a numeric value.
final case class Num(value: Double) extends Expr

// A unary operation expression (eg. -5 is UnOp("-", Num(5))).
final case class UnOp(op: String, value: Expr) extends Expr

// A binary operation expression (eg. 2+3 is BinOp("+", Num(2), Num(3))))
final case class BinOp(op: String, left: Expr, right: Expr) extends Expr

// The calculator object.
object Calculator {

  // Simplifies the head of the expression (should not simplify recursively!).  
  def simplifyHead(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => e
    case BinOp("+", e, Num(0)) => e
    case BinOp("+", Num(0), e) => e
    case BinOp("*", e, Num(1)) => e
    case BinOp("*", Num(1), e) => e
    case BinOp("*", e, Num(0)) => Num(0)
    case BinOp("*", Num(0), e) => Num(0)
    case BinOp("-", e, q) if (q == e)  => Num(0)
    case BinOp("+", Num(a), Num(b)) => Num(a + b)
    case BinOp("-", Num(a), Num(b)) => Num(a - b)
    case BinOp("*", Num(a), Num(b)) => Num(a * b)
    case BinOp(op, Var("DUP"), Var("DUP")) => Num(0)
    case BinOp(op, Var("DUP"), e) => BinOp(op, e, e)
    case BinOp(op, e, Var("DUP")) => BinOp(op, e, e)
    case _ => expr
  }


  
    // Evaluates the expression to a numeric value.
    def evaluate(expr: Expr): Double = expr match{

      case Num(a) => a
      case Var(b) => 1
      case UnOp("-", Num(a)) => -a 
      case UnOp("-", a) => evaluate(UnOp("-", Num(evaluate(simplifyHead(a)))))
      case UnOp(op, a) => 1
      case BinOp(op, Var("DUP"), e) => evaluate(simplifyHead(expr))
      case BinOp(op, e, Var("DUP")) => evaluate(simplifyHead(expr))
      case BinOp(op, l, r) if (op != "+" && op != "-" && op != "*" ) => 1
      case BinOp(op, Num(a), Num(b)) => evaluate(simplifyHead(expr))
      case BinOp(op, l, r) => evaluate(BinOp(op, Num(evaluate(simplifyHead(l))), Num(evaluate(simplifyHead(r)))))

    
    }

}
