# Stacker &#x1f95e;

Stacker is a mixed static/runtime analyzer that runs the dependents of a Cabal package of interest to find out how it's used in the wild. Stacker creates a dependency graph of values starting from a function call from a package dependent.

**Try it live at <http://stacker.lam.io>!**

Stacker's main goal is to address barriers to adoption of packages and frameworks that arises from skepticism about examples and The Right Way. Instead, let's prefer collecting hard statistics on how people _actually_ use the API. Reciprocally, it also helps package owners identify kludges in their interface.

Stacker uses GHC's [cost-centre profiler](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#cost-centres-and-cost-centre-stacks) to collect a stack trace from executing tests on a package dependent. It then analyzes [HIE (Haskell Interface Extended) files](https://gitlab.haskell.org/ghc/ghc/-/wikis/hie-files) that contain the ASTs of the sources in the compilation set. In the AST, Stacker follows dependencies of the target package's functions up the call stack and along value bindings to backtrack the arguments to the function of interest as well.

_Note: HIE files are a new feature of **GHC 8.8**._

Try running stacker on itself!

1. `./run` to build the project in `target` which will be analyzed on the first cycle.
1. Open `target/stacker-fake-pkg.prof` and find the cost centre ID for `main`.
1. Grab the "Cost centre no." (the fourth column, just after the source location).
1. Build and run stacker against the target with `./run <main-cost-centre-num> target/stacker-fake-pkg.prof target/hie`.
1. Run stacker against itself subsequently by:
   1. Opening `./stacker.prof` and finding the cost center # of a function call of interest;
   1. Stashing the `.prof` file with `$ mv stacker.prof stacker.prof.bak`;
   1. `./run <cost-centre-num> stacker.prof.bak hie vendor`
   1. You should see a list of graph edges in the dependency graph.

Generally, `run` accepts a cost center number, the `.prof` file and any number of directories with HIE files. Stacker built with `run` will generate HIE files for all its dependencies under `vendor/hie`.

(2020-08-04) Tests + a usable shell/web UI are underway. The results and input process are changing shortly.