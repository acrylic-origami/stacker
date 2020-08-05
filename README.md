# Stacker &#x1f95e;

Stacker is a mixed static/runtime analyzer that runs the dependents of a Cabal package of interest to find out how it's used in the wild. Stacker creates a dependency graph of values starting from a function call from a package dependent.

Stacker's main goal is to address barriers to adoption of packages and frameworks that arises from skepticism about examples and The Right Way. Instead, let's prefer collecting hard statistics on how people _actually_ use the API. Reciprocally, it also helps package owners identify kludges in their interface.

Stacker uses GHC's [cost-centre profiler](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#cost-centres-and-cost-centre-stacks) to collect a stack trace from executing tests on a package dependent. It then analyzes [HIE (Haskell Interface Extended) files](https://gitlab.haskell.org/ghc/ghc/-/wikis/hie-files) that contain the ASTs of the sources in the compilation set. In the AST, Stacker follows dependencies of the target package's functions up the call stack and along value bindings to backtrack the arguments to the function of interest as well.

_Note: HIE files are a new feature of **GHC 8.8**._

Try running stacker on itself!

1. Build stacker first to produce HIE and prof files.
1. Open `stacker.prof` and find a function call of interest.
1. Grab the "Cost centre ID" (the fourth column, just after the source location)
1. Run:

    ```bash
    $ mv stacker.prof stacker.prof.bak
    $ cabal run stacker -- <cost-center-id> stacker.prof.bak hie/
    ```

You should see a list of graph edges in the dependency graph.

(2020-08-04) Tests + a usable shell/web UI are underway. The results and input process are changing shortly.