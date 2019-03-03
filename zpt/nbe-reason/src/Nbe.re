type typ =
  | Base
  | Arr(typ, typ);

type term =
  | Var(string)
  | Abs(string, typ, term)
  | App(term, term);

type semantics =
  | MNat(int)
  | MAbs(semantics => semantics);

type environment = string => semantics;

type exn +=
  | Undefined_variable;

let envEmpty: environment = _ => raise(Undefined_variable);

let envExt: (string, semantics, environment) => environment =
  (name, sem, env) => (name2) =>
    if (name === name2) {
      sem;
    } else {
      env(name2);
    };

type exn +=
  | Wrong_application;

let rec eval: (term, environment) => semantics =
  (tm, env) => {
    switch (tm) {
    | Var(name) => env(name)
    | Abs(name, _, n) => MAbs(sem => evaluate(n, envExt(name, sem, env)))
    | App(n1, n2) =>
      let sem1 = evaluate(n1, env);
      let sem2 = evaluate(n2, env);

      switch (sem1) {
      | MAbs(f) => f(sem2)
      | MNat(_) => raise(Wrong_application)
      };
    };
  };

let varCount = ref(0)
let gensym: unit => string =
  () => {
    let num = varCount^;
    varCount := num + 1;
    "x" ++ Js.Int.toString(num)
  }

type exn +=
  | Illegal_type;

let rec reify: (semantics, typ) => term =
  (sem) => (ty) => {
    switch (sem) {
    | MBase(tm) => tm
    | MAbs(fn) => 
      switch (ty) {
      | Base => raise(Illegal_type)
      | Arr(ty1, ty2) =>
        let x = gensym();
        Abs(x, ty1, reify(fn(reflect(Var(x), ty1)), ty2))
      }
    }
  }
and reflect: (term, typ) => semantics =
  (tm, ty) => {
    switch(ty) {
    | Base => MBase(tm)
    | Arr(ty1, ty2) =>
      MAbs((sem) => reflect(App(tm, reify(sem, ty1)), ty2))
    }
  }
