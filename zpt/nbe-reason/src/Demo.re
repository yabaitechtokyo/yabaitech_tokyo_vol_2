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
| MAbs(semantics => option(semantics))
;

type env = string => semantics;

let envExt : (env, string, semantics) => env =  (env, name, sem) => {
    (name2) => if (name === name2) {sem} else {env(name2)}
}

let rec evaluate: term => env => option(semantics) = (m, e) => {
    switch (m) {
        | Var(name) => Some(e(name))
        | Abs(name, _, n) => {
            Some(MAbs((sem) => evaluate(n, envExt(e, name, sem))))
        }
        | App(n1, n2) => {
            let sem1opt = evaluate(n1, e);
            let sem2opt = evaluate(n2, e);

            switch sem1opt {
                | Some(sem1) => {
                    switch sem2opt {
                        | Some(sem2) => {
                            switch sem1 {
                                | MAbs(f) => f(sem2)
                                | MNat(_) => None
                            }
                        }
                        | None => None
                    } 
                }
                | None => None
            }
        }
    }
}