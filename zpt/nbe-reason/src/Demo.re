type typ = 
| Base
| Arr(typ, typ)
;

type term =
| Var(string)
| Abs(string, typ, term)
| App(term, term)
;

type semantics =
| MNat(int)
| MAbs(semantics => semantics)
;

type env = string => semantics;

let envExt : (env, string, semantics) => env =  (env, name, sem) => {
    (name2) => if (name === name2) {sem} else {env(name2)}
}

type exn += Wrong_application;

let rec evaluate: term => env => semantics = (m, e) => {
    switch (m) {
        | Var(name) => e(name)
        | Abs(name, _, n) => MAbs((sem) => evaluate(n, envExt(e, name, sem)))
        | App(n1, n2) => {
            let sem1 = evaluate(n1, e);
            let sem2 = evaluate(n2, e);

            switch sem1 {
                | MAbs(f) => f(sem2)
                | MNat(_) => raise(Wrong_application)
            }
        }
    }
}