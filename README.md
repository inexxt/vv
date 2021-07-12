# vv

`vv` is a functional language with dependent types - essentially, implementation of the Calculus of Constructions. 
It comes with built-in dependent products and universes, and only then the standard library implements all of the required functionality, such as numbers, arithmetic, logical operators, products, sums, lists, empty type, Option etc. Thus, there is a very simple core language with well-understood behaviour, but powerful enough as to not need the implementation of these all features in the core. All typechecking programs written in `vv` terminate.

## Modules
Code that is to be imported should have `.vvm` extension. Code that implements `main` should have `.vv` extension. You cannot import `main` from somewhere else.

## Running the code

Compilation is done by `make`. To compile, `bnfc` has to be in the `PATH`.

Running the interpreter is done by executing `./interpreter FILENAME`. Setting the verbose flag `-v` for the interpreter is recommended, since it's sometimes not clear what is going wrong in the code without it.

In the unlikely case the standard library cannot be found (e.g. missing import errors), its location can be specified using `-s` option.

## Syntax highlighting

Syntax highlighting scheme for the `.vv` and `.vvm` files is provided for Sublime Text 3. The editor can be downloaded for free from https://www.sublimetext.com/3. To install the highlighting, simply copy the contents of `syntax_highlighting` folder into `~/.config/sublime-text-3/Packages/vv/VV.sublime-syntax`.

Highlighting massively improves readability, as shown beloww, so I really recommend enabling it.
![code](https://github.com/inexxt/JPP/blob/master/vv/code.png?raw=true)

## Reading the output

Because a lot of the functionality is implemented in the user-space, in standard library, I it was a conscious decision not to include any syntactic sugar for printing the values.  
This means that that, among others:  
 - Natural numbers are printed in the Peano notation, for example 3 = S (S (S Z))
 - Lists are printed using the provided constants: empty list as `e`, cons as `((c tail) head)`.

## Examples
The majority of the non-trivial code is presented in the standard library in `stdlib/`. The `examples` directory contains `good` and `bad` subdirs, which are presenting the constructs of `vv` on very small, simple examples.  
I list the order in which I recommed reading them below. Examples are somewhat commented.
 - `EightyOne.vv`
 - `IfZero.vv`
 - `Minus.vv`
 - `NatEquality.vv`
 - `ProductType.vv`
 - `BasicList.vv`
 - `ListLength.vv`
 - `ListLengthLazy.vv`
 - `GeneratedList.vv`
 - `GeneratedListFilter.vv`
 - `ListFold.vv`
and then the library files:
 - `Type.vvm`
 - `Bool.vvm`
 - `Empty.vvm`
 - `Pair.vvm`
 - `Nat.vvm`
 - `Arith.vvm`
 - `List.vvm`
 - `Option.vvm`
 - `NConst.vvm`
 - `Union.vvm`


## What doesn't work (yet)

There's a few things that don't work (yet, or in general):
  1) Type annotations in the `let` and top level are mostly cosmetic. 
     This means that for example, the following code typechecks correctly:
     ```assume N : Type ;;
        assume M : Type ;;
        assume x : N ;;
        
        y : M ;
        y = x ;;
```
    I want to explicitly mention that this does not mean that the typechecker doesn't work! 
    It does, and it infers the type of `y` correctly, but it just doesn't check that it is indeed equal to the one specified by the user.
    This is because of some implementation choices that made the refactor time-consuming and I didn't finish.
  
  2) There are no line numbers when showing errors in the code (except for parsing). 
     This is because the substitution process in global, so it is very difficult to pinpoint the location of the error (I can explain more in person).

  3) The `Union` type might not work correctly, I didn't have time to test it.
  
  4) I didn't finish working on the grammar, so there are multiple undocumented conflicts.