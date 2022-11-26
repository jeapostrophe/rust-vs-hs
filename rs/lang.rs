// Language
enum Prim {
  PAdd,
  PMul,
  PDiv,
  PNumToStr,
}

enum Val {
  VNum(i32),
  VStr(String),
}

enum Expr {
  EVar(String),
  EVal(Val),
  EApp(Prim, Vec<Expr>),
  ELet(String, Box<Expr>, Box<Expr>),
  EDbg(Box<Expr>),
}

// Evaluator
type EvalT = Result<Val, String>;

trait Eval {
  fn eval(&self) -> EvalT;
}

impl Eval for Expr {
  fn eval(&self) -> EvalT {
    use Expr::*;
    match self {
      EVal(v) => Ok(*v),
    }
  }
}

// Examples
fn main() {
  use Prim::*;
  use Val::*;
  use Expr::*;

  let x1 = "x".to_string();
  let x2 = "x".to_string();
  let x3 = "x".to_string();
  let y1 = "y".to_string();
  let y2 = "y".to_string();

  let e1 =
    ELet(x1, Box::new(EVal(VNum(5))),
    Box::new(ELet(y1, Box::new(EVal(VNum(4))),
    Box::new(EDbg(
      Box::new(EApp(PAdd, vec![
           EVar(x2),
           EApp(PMul, vec![
                EVar(x3),
                EVar(y2),
           ]),
      ]))
    )))));

  let e2 =
    EApp(PAdd, vec![
         EApp(PNumToStr, vec![
              EVal(VNum(5)),
         ]),
         EVal(VNum(5)),
    ]);

  let e3 =
    EApp(PDiv, vec![
         EVal(VNum(5)),
         EVal(VNum(0)),
    ]);
}
