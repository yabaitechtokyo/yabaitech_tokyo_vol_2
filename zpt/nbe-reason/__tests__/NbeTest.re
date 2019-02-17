open Nbe;

let id = Abs("x", Base, Var("x"));

// (a -> a) -> a -> a
let nat = Arr(Arr(Base, Base), Arr(Base, Base));
let zero = Abs("f", Arr(Base, Base), Abs("x", Base, Var("x")));
let one =
  Abs("f", Arr(Base, Base), Abs("x", Base, App(Var("f"), Var("x"))));
// plus := \n \m \f \x n f (m f x)
let plus =
  Abs(
    "n",
    nat,
    Abs(
      "m",
      nat,
      Abs(
        "f",
        Arr(Base, Base),
        Abs(
          "x",
          Base,
          App(
            App(Var("n"), Var("f")),
            App(App(Var("n"), Var("bf")), Var("x")),
          ),
        ),
      ),
    ),
  );
// times := \n \m \f n (m f)
let times =
  Abs(
    "n",
    nat,
    Abs(
      "m",
      nat,
      Abs("f", Arr(Base, Base), App(Var("n"), App(Var("m"), Var("f")))),
    ),
  );

type exn +=
  | Runtime_type_error;

let inc1: semantics =
  MAbs(
    sem =>
      switch (sem) {
      | MNat(n) => MNat(n + 1)
      | MAbs(_) => raise(Runtime_type_error)
      },
  );

let testEnv = envEmpty |> envExt("f", inc1) |> envExt("x", MNat(0));

// test: 1 + 1 = 2
let two = App(App(plus, one), one);
evaluate(App(App(two, Var("f")), Var("x")), testEnv);

open Jest;
open Expect;

describe("true", () =>
  test("true is true", () =>
    expect(true) |> toBe(true)
  )
);