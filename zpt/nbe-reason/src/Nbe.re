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

type exn +=
  | Undefined_variable;

type env = string => semantics;

let envEmpty: env = _ => raise(Undefined_variable);

let envExt: (string, semantics, env) => env =
  (name, sem, env, name2) =>
    if (name === name2) {
      sem;
    } else {
      env(name2);
    };

type exn +=
  | Wrong_application;

let rec evaluate: (term, env) => semantics =
  (m, e) => {
    switch (m) {
    | Var(name) => e(name)
    | Abs(name, _, n) => MAbs(sem => evaluate(n, envExt(name, sem, e)))
    | App(n1, n2) =>
      let sem1 = evaluate(n1, e);
      let sem2 = evaluate(n2, e);

      switch (sem1) {
      | MAbs(f) => f(sem2)
      | MNat(_) => raise(Wrong_application)
      };
    };
  };