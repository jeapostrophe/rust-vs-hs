use wasm_bindgen::prelude::*;
use im_rc::hashmap::*;
use im_rc::hashmap;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}
macro_rules! console_log {
  ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

#[derive(Clone, Copy, Debug)]
enum Prim {
  Add,
  Mul,
  Div,
  NumToStr,
  Lt,
}

#[derive(Clone, Debug)]
enum Val {
  Num(i32),
  Bool(bool),
  Str(String),
}

#[derive(Clone, Debug)]
enum Expr {
  Var(String),
  Ret(Val),
  App(Prim, Vec<Expr>),
  Let(String, Box<Expr>, Box<Expr>),
  If(Box<Expr>, Box<Expr>, Box<Expr>),
  Fail(String),
  Dbg(Box<Expr>),
}

// Evaluator
type EvalT = Result<Val, String>;

struct EvalEnv {
  env: HashMap<String, Val>,
}

trait Eval {
  fn eval(&self, ee:&EvalEnv) -> EvalT;
}

struct PrimApp { p: Prim, vs: Vec<Val> }
impl Eval for PrimApp {
  fn eval(&self, _ee: &EvalEnv) -> EvalT {
    use Prim::*; use Val::*;
    match (self.p, &self.vs[..]) {
      (Lt, [ Num(xn), Num(yn) ]) => Ok(Bool(xn<yn)),
      (Add, [ Num(xn), Num(yn) ]) => Ok(Num(xn+yn)),
      (Mul, [ Num(xn), Num(yn) ]) => Ok(Num(xn*yn)),
      (Div, [ Num(xn), Num(yn) ]) => {
        if *yn == 0 {
          Err("div by zero".to_string())
        } else {
          Ok(Num(xn/yn))
        }
      },
      (NumToStr, [ Num(n) ]) => Ok(Str(n.to_string())),
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
    use Expr::*; use Val::*;
    match self {
      Var(v) => v.eval(ee),
      Ret(v) => Ok(v.clone()),
      App(p, args) => {
        let rvs: Result<Vec<_>, _> = args.iter().map(|x| x.eval(ee)).collect();
        (PrimApp { p: *p, vs: rvs? }).eval(ee)
      },
      Let(x, xe, e) => {
        let xv = xe.eval(ee)?;
        let ep = EvalEnv {
          env: ee.env.update(x.clone(), xv),
        };
        e.eval(&ep)
      },
      If(c, t, f) => {
        match c.eval(ee)? {
          Bool(false) => f.eval(ee),
          _ => t.eval(ee),
        }
      },
      Fail(s) => Err(s.clone()),
      Dbg(xe) => {
        console_log!("=> {:?}", xe);
        let xv = xe.eval(ee)?;
        console_log!("<= {:?}", xv);
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

  fn run(&self, e: &Expr) {
    match e.eval(self) {
      Ok(v) => console_log!("Succ: {:?}", v),
      Err(s) => console_log!("Fail: {:?}", s),
    };
  }
}

#[wasm_bindgen]
pub fn run_examples() {
  use Val::*; use Expr::*; use Prim::*;

  let e = EvalEnv::new();

  let x = "x".to_string();
  let y = "y".to_string();

  let e1 =
    Let(x.clone(), Box::new(Ret(Num(5))),
    Box::new(Let(y.clone(), Box::new(Ret(Num(4))),
    Box::new(Dbg(
      Box::new(App(Add, vec![
           Var(x.clone()),
           App(Mul, vec![
                Var(x.clone()),
                Var(y.clone()),
           ]),
      ]))
    )))));
  e.run(&e1);

  let e1b = 
    Let(x.clone(), Box::new(Ret(Num(7))),
      Box::new(App(Add, vec![
        e1,
        Var(x.clone())
    ])));
  e.run(&e1b);
    
  let e2 =
    App(Add, vec![
         App(NumToStr, vec![
              Ret(Num(5)),
         ]),
         Ret(Num(5)),
    ]);
  e.run(&e2);

  let e3 =
    App(Div, vec![
         Ret(Num(5)),
         Ret(Num(0)),
    ]);
  e.run(&e3);
}
