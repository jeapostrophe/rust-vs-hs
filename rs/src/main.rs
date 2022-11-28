use std::borrow::Borrow;
use std::rc::Rc;
use im_rc::hashmap::*;
use im_rc::hashmap;

#[derive(Debug)]
enum Prim {
  PAdd,
  PMul,
  PDiv,
  PNumToStr,
}
use Prim::*;

#[derive(Debug)]
enum Val {
  VNum(i32),
  VStr(Rc<String>),
}
use Val::*;

#[derive(Debug)]
enum Expr {
  EVar(Rc<String>),
  EVal(Rc<Val>),
  EApp(Prim, Vec<Rc<Expr>>),
  ELet(Rc<String>, Rc<Expr>, Rc<Expr>),
  EDbg(Rc<Expr>),
}
use Expr::*;

// Evaluator
type EvalT = Result<Rc<Val>, String>;

struct EvalEnv {
  env: HashMap<Rc<String>, Rc<Val>>,
}

trait Eval {
  fn eval(&self, ee:&EvalEnv) -> EvalT;
}

impl Prim {
  fn eval_prim(&self, _ee: &EvalEnv, vs:Vec<Rc<Val>>) -> EvalT {
    match (self, &vs[..]) {
      (PAdd, [ x, y ]) => {
        match (x.borrow(), y.borrow()) {
          (VNum(xn), VNum(yn)) => Ok(Rc::new(VNum(xn+yn))),
        _ => Err("add: not nums".to_string()),
        }
      },
      (PMul, [ x, y ]) => {
        match (x.borrow(), y.borrow()) {
          (VNum(xn), VNum(yn)) => Ok(Rc::new(VNum(xn*yn))),
        _ => Err("mul: not nums".to_string()),
        }
      },
      (PDiv, [ x, y ]) => {
        match (x.borrow(), y.borrow()) {
          (VNum(xn), VNum(yn)) => {
            if *yn == 0 {
              Err("div by zero".to_string())
            } else {
              Ok(Rc::new(VNum(xn/yn)))
            }
          },
        _ => Err("div: not nums".to_string()),
        }
      },
      (PNumToStr, [ x ]) => {
        match **x {
          VNum(n) => Ok(Rc::new(VStr(Rc::new(format!("{:?}", n))))),
          _ => Err("numToStr: not num".to_string()),
        }
      },
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
        let rvs: Result<Vec<Rc<Val>>, String> = args.into_iter().map(|x| x.eval(ee)).collect();
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

  fn run(&self, e: Rc<Expr>) -> () {
    match e.eval(self) {
      Ok(v) => println!("Succ: {:?}", v),
      Err(s) => println!("Fail: {:?}", s),
    };
  }
}

// Examples
fn main() {
  let e = EvalEnv::new();

  let x = Rc::new("x".to_string());
  let y = Rc::new("y".to_string());

  let e1 =
    Rc::new(ELet(x.clone(), Rc::new(EVal(Rc::new(VNum(5)))),
    Rc::new(ELet(y.clone(), Rc::new(EVal(Rc::new(VNum(4)))),
    Rc::new(EDbg(
      Rc::new(EApp(PAdd, vec![
           Rc::new(EVar(x.clone())),
           Rc::new(EApp(PMul, vec![
                Rc::new(EVar(x.clone())),
                Rc::new(EVar(y.clone())),
           ])),
      ]))
    ))))));
  e.run(e1.clone());

  let e1b = 
    Rc::new(ELet(x.clone(), Rc::new(EVal(Rc::new(VNum(7)))),
      Rc::new(EApp(PAdd, vec![
        e1.clone(),
        Rc::new(EVar(x.clone()))
    ]))));
  e.run(e1b);
    
  let e2 =
    Rc::new(EApp(PAdd, vec![
         Rc::new(EApp(PNumToStr, vec![
              Rc::new(EVal(Rc::new(VNum(5)))),
         ])),
         Rc::new(EVal(Rc::new(VNum(5)))),
    ]));
  e.run(e2);

  let e3 =
    Rc::new(EApp(PDiv, vec![
         Rc::new(EVal(Rc::new(VNum(5)))),
         Rc::new(EVal(Rc::new(VNum(0)))),
    ]));
  e.run(e3);
}
