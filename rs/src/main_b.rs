use im_rc::hashmap::*;
use im_rc::hashmap;

#[derive(Clone, Copy, Debug)]
enum Prim {
  PAdd,
  PMul,
  PDiv,
  PNumToStr,
}
use Prim::*;

#[derive(Clone, Debug)]
enum Val {
  VNum(i32),
  VStr(String),
}
use Val::*;

#[derive(Clone, Debug)]
enum Expr {
  EVar(String),
  EVal(Val),
  EApp(Prim, Vec<Expr>),
  ELet(String, Box<Expr>, Box<Expr>),
  EDbg(Box<Expr>),
}
use Expr::*;

// Evaluator
type EvalT = Result<Val, String>;

struct EvalEnv {
  env: HashMap<String, Val>,
}

trait Eval {
  fn eval(&self, ee:&EvalEnv) -> EvalT;
}

impl Prim {
  fn eval_prim(&self, _ee: &EvalEnv, vs:Vec<Val>) -> EvalT {
    match (self, &vs[..]) {
      (PAdd, [ VNum(xn), VNum(yn) ]) => Ok(VNum(xn+yn)),
      (PMul, [ VNum(xn), VNum(yn) ]) => Ok(VNum(xn*yn)),
      (PDiv, [ VNum(xn), VNum(yn) ]) => {
        if *yn == 0 {
          Err("div by zero".to_string())
        } else {
          Ok(VNum(xn/yn))
        }
      },
      (PNumToStr, [ VNum(n) ]) => Ok(VStr(format!("{:?}", n))),
      _ => Err("prim wrong args".to_string()),
    }
  }
}

impl Eval for String {
  fn eval(&self, ee: &EvalEnv) -> EvalT {
    match ee.env.get(self) {
      None => Err(format!("Unbound variable: {:?}", self)),
      Some(v) => Ok(v.clone()),
    }
  }
}

impl Eval for Expr {
  fn eval(&self, ee: &EvalEnv) -> EvalT {
    match self {
      EVar(v) => v.eval(ee),
      EVal(v) => Ok(v.clone()),
      EApp(p, args) => {
        let rvs: Result<Vec<Val>, String> = args.into_iter().map(|x| x.eval(ee)).collect();
        let vs = rvs?;
        p.eval_prim(ee, vs)
      },
      ELet(x, xe, e) => {
        let xv = xe.eval(ee)?;
        let ep = EvalEnv {
          env: ee.env.update(x.clone(), xv),
        };
        e.eval(&ep)
      }
      EDbg(xe) => {
        println!("=> {:?}", xe);
        let xv = xe.eval(ee)?;
        println!("<= {:?}", xv);
        Ok(xv)
      },
    }
  }
}

impl EvalEnv {
  fn new() -> EvalEnv {
    EvalEnv {
      env: hashmap!{},
    }
  }

  fn run(&self, e: &Expr) -> () {
    match e.eval(self) {
      Ok(v) => println!("Succ: {:?}", v),
      Err(s) => println!("Fail: {:?}", s),
    };
  }
}

// Examples
fn main() {
  let e = EvalEnv::new();

  let x = "x".to_string();
  let y = "y".to_string();

  let e1 =
    ELet(x.clone(), Box::new(EVal(VNum(5))),
    Box::new(ELet(y.clone(), Box::new(EVal(VNum(4))),
    Box::new(EDbg(
      Box::new(EApp(PAdd, vec![
           EVar(x.clone()),
           EApp(PMul, vec![
                EVar(x.clone()),
                EVar(y.clone()),
           ]),
      ]))
    )))));
  e.run(&e1);

  let e1b = 
    ELet(x.clone(), Box::new(EVal(VNum(7))),
      Box::new(EApp(PAdd, vec![
        e1,
        EVar(x.clone())
    ])));
  e.run(&e1b);
    
  let e2 =
    EApp(PAdd, vec![
         EApp(PNumToStr, vec![
              EVal(VNum(5)),
         ]),
         EVal(VNum(5)),
    ]);
  e.run(&e2);

  let e3 =
    EApp(PDiv, vec![
         EVal(VNum(5)),
         EVal(VNum(0)),
    ]);
  e.run(&e3);
}
