Evaluation
==========

This document explains how to run the evaluations that come in this repository.

First, make sure that you are able to compile the connection provers.
To check this, run `cargo build --release` in this directory.

Next, you require the relevant first-order problem datasets.
There are a couple of `Makefile`s in the `i` directory that automate getting these.
Running the following commands in the `i` directory should obtain all datasets:

    make -f mptp2078.mk bushy
    make -f mptp2078.mk chainy
    make -f miz40.mk miz40-deps.a15
    make -f flyspeck.mk flyspeck-top
    make -f tptp.mk TPTP-v6.3.0

Now we are ready to get rolling.

To evaluate
on all datasets with 1 second timeout
the meanCoP strategies using all possible combination of cuts,
using 40 cores simultaneously:

    SETS={bushy,chainy,TPTP-v6.3.0,miz40-deps.a15,flyspeck-top}
    CFGS=meancop--conj{,--cutred}{,--cutextshallow,--cutextdeep}
    eval make o/$SETS/1s/$CFGS -j40

Then, to generate the sets of solved files per strategy:

    eval make -f solved.mk solved/$SETS/1s/$CFGS

Substitute `1s` by `10s` to run the same evaluation with 10 seconds timeout.[^10s]
Note that when evaluating all cut combinations with 10 seconds timeout,
the FS-top and Miz40 datasets take about 7 hours each (!), whereas
bushy, chainy, and TPTP require only roughly one hour each.

The output consists of several kinds of files:

* `*.p`: Standard output of the prover.
* `*.p.time`: Runtime information (in JSON format).
* `*.p.infs`: Number of inferences per path limit (JSON).
* `*.p.o`: Proofs.
