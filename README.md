# Smyth

Smyth is a program synthesizer that will fill in "holes" in your program when
given input-output examples, or, more generally, arbitrary assertions about the
behavior of the code. These assertions are just "normal code" and can serve as
documentation and unit tests once the synthesis has completed.

Evaluation of these assertions internally gives rise to input-output examples
which, along with the types in the program, guide the synthesis search to fill
in the holes. The key technical innovation, _live bidirectional evaluation_,
propagates examples "backward" through partially evaluated _sketches_ (that is,
programs with holes).

Live bidirectional evaluation enables Smyth not only to specify and solve
interdependent synthesis goals over holes at arbitrary locations in the program,
it also removes the need to specify "trace-complete" sets of examples recursive
functions (that is, sets of examples that mirror the intended recursive behavior
of the function to be synthesized), a major limitation of prior work on
evaluator-based synthesis.

A formal exposition of the system (including its evaluation and underlying
theory) can be found in our ICFP 2020, _Program Sketching with Live
Bidirectional Evaluation_.

## Installation

1. Install `opam`, the OCaml package manager:

     https://opam.ocaml.org/doc/Install.html

2. Install OCaml >=4.08.1 with the
   [flambda](https://caml.inria.fr/pub/docs/manual-ocaml/flambda.html)
   optimizer by running `opam switch create 4.08.1+flambda`.

3. Run `make deps` in the root directory of this project to download all the
   necessary `opam` dependencies.

4. Run `make` to build the Smyth executable. The executable is accessible via
   the `smyth` symlink in the root directory of this project.

To replicate the experiments we have run to evaluate Smyth, you will also need
the following tools installed:

  - GNU Octave
  - LaTeX
  - Python 2.7
  - Python 3

## Synthesizing a Program

The `./smyth forge` command takes in a path to a sketch to be completed
("forged") and outputs its result.

We have provided the six sketches described in Sections 1 and 2 of our ICFP
paper in the `examples/` directory so that you can, for example, run `./smyth
forge examples/stutter.elm` from the `smyth/` directory to see the output. Feel
free to change the input-output examples on these sketches (or the program
sketches themselves) to see how `smyth` responds. You can also create your own
files following the same syntax (mostly Elm) to try out.

By default `./smyth forge` only shows the top solution, but after all the other
arguments, you can pass in the flag `--show=top1r` or `--show=top3` to show the
top recursive solution or top three overall solutions, respectively.

The benchmark sketches from the experimental evaluation of Smyth are stored in
`suites/SUITE_NAME/sketches` and the input-examples used for those sketches are
stored in `smyth/suites/SUITE_NAME/examples`. If you want to see the actual
synthesis output on just one of these sketches, you can use the `forge` helper
script that we have provided in the `smyth/` directory as follows:

  `./forge <top1|top1r|top3> <suite-name> <sketch-name>`

So, for example, you could run

  `./forge top1 no-sketch list_sorted_insert`

to see the top result of running the `list_sorted_insert` benchmark from the
`no-sketch` suite.

## Running the Experimental Evaluation

_*Quick summary*: navigate to `experiments/`, run `./run-all 10`, look at
`smyth/experiments/latex-tables/smyth-experiment-tables.pdf`_

To run all the data collection and analysis for Smyth as described in our ICFP
paper, navigate to the `experiments/` directory and run the script `run-all`,
which takes a positive integer `N` as an argument that indicates how many trials
per example-set size to run for each benchmark in Experiment 2b and 3b (the
randomized experiments).

We ran these experiments with `N = 50` (that is, `./run-all 50`), which takes
about 2 to 2.5 hours to run on a Mid 2012 MacBook Pro with a 2.5 GHz Intel Core
i5 CPU and 16 GB of RAM.

If you would prefer to get the results faster (at the loss of statistical
precision), we would recommend running the experiments with `N = 10` (that is,
`./run-all 10`), which takes about 30 minutes to run on the same MacBook.

Once this script is complete, you can take a look at the results with any
phenomena of note explained in
`experiments/latex-tables/smyth-experiment-tables.pdf`.

*Note:* Rarely, due to timing issues with the UNIX Timer API, one of the
benchmarks that is not indicated as a failure might crash with a "Timeout"
exception during Experiments 2b and 3b.  Even more rarely, a stack overflow
error might occur. To combat these situations, the Experiment 2b and 3b scripts
automatically retry a benchmark on unexpected failure.

## The Codebase

The following table provides a roadmap of where each concept/figure in the paper
can be found in the codebase, in order of presentation in the paper.

To see how these all fit together with actual code, you can view the "synthesis
pipeline" in [`endpoint.ml`](smyth/lib/smyth/endpoint.ml) (specifically, the `solve`
function).

| Concept/Figure                              | File (in [`smyth/lib/smyth/`](smyth/lib/smyth/))
| ------------------------------------------- | ------------------------------
| Syntax of Core Smyth                        | [`lang.ml`](smyth/lib/smyth/lang.ml)
| Type checking                               | [`type.mli`](smyth/lib/smyth/type.mli)/[`type.ml`](smyth/lib/smyth/type.ml)
| Expression evaluation                       | [`eval.mli`](smyth/lib/smyth/eval.mli)/[`eval.ml`](smyth/lib/smyth/eval.ml)
| Resumption                                  | [`eval.mli`](smyth/lib/smyth/eval.mli)/[`eval.ml`](smyth/lib/smyth/eval.ml)
| Example satisfaction                        | [`example.mli`](smyth/lib/smyth/example.mli)/[`example.ml`](smyth/lib/smyth/example.ml)
| Constraint satisfaction                     | [`constraints.mli`](smyth/lib/smyth/constraints.mli)/[`constraints.ml`](smyth/lib/smyth/constraints.ml)
| Constraint merging                          | [`constraints.mli`](smyth/lib/smyth/constraints.mli)/[`constraints.ml`](smyth/lib/smyth/constraints.ml)
| Live bidirectional example checking         | [`uneval.mli`](smyth/lib/smyth/uneval.mli)/[`uneval.ml`](smyth/lib/smyth/uneval.ml)
| Example unevaluation                        | [`uneval.mli`](smyth/lib/smyth/uneval.mli)/[`uneval.ml`](smyth/lib/smyth/uneval.ml)
| Program evaluation                          | [`eval.mli`](smyth/lib/smyth/eval.mli)/[`eval.ml`](smyth/lib/smyth/eval.ml)
| Result consistency                          | [`res.mli`](smyth/lib/smyth/res.mli)/[`res.ml`](smyth/lib/smyth/res.ml)
| Assertion satisfaction and simplification   | [`uneval.mli`](smyth/lib/smyth/uneval.mli)/[`uneval.ml`](smyth/lib/smyth/uneval.ml)
| Constraint simplification                   | [`solve.mli`](smyth/lib/smyth/solve.mli)/[`solve.ml`](smyth/lib/smyth/solve.ml)
| Constraint solving                          | [`solve.mli`](smyth/lib/smyth/solve.mli)/[`solve.ml`](smyth/lib/smyth/solve.ml)
| Type-and-example-eirected hole synthesis    | [`fill.mli`](smyth/lib/smyth/fill.mli)/[`fill.ml`](smyth/lib/smyth/fill.ml)
| Type-directed guessing (term generation)    | [`term_gen.mli`](smyth/lib/smyth/term_gen.mli)/[`term_gen.ml`](smyth/lib/smyth/term_gen.ml)
| Type-and-example-directed refinement        | [`refine.mli`](smyth/lib/smyth/refine.mli)/[`refine.ml`](smyth/lib/smyth/refine.ml)
| Type-and-example-directed branching         | [`branch.mli`](smyth/lib/smyth/branch.mli)/[`branch.ml`](smyth/lib/smyth/branch.ml)
