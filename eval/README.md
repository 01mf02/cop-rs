Evaluation
==========

This document explains how to run the evaluations that come in this repository.

Preparation
-----------

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

Evaluation
----------

Now we are ready to get rolling.

To evaluate
on all datasets with 1 second timeout
the meanCoP strategies using all possible combination of cuts,
using 40 cores simultaneously:

    SETS={bushy,chainy,TPTP-v6.3.0,miz40-deps.a15,flyspeck-top}
    CFGS=meancop--conj{,--cuts{ei,ex},--cutsr{,ei,ex}}
    eval make o/$SETS/1s/$CFGS -j40

Then, to generate the sets of solved files per strategy:

    eval make -f solved.mk solved/$SETS/1s/$CFGS

Substitute `1s` by `10s` to run the same evaluation with 10 seconds timeout.[^10s]
Note that when evaluating all cut combinations with 10 seconds timeout,
the FS-top and Miz40 datasets take about 7 hours each (!), whereas
bushy, chainy, and TPTP require only roughly one hour each.

Once you have a set of solved files (in the `solved` directory),
you can run the evaluation on the solved files only.
For this, set `USE_SOLVED` as follows:

    eval make USE_SOLVED=1 o/$SETS/1s/$CFGS -j40

Output
------

The output consists of several kinds of files:

* `*.p`: Standard output of the prover.
* `*.p.time`: Runtime information (in JSON format).
* `*.p.stats`: Number of inferences per path limit (JSON).
* `*.p.o`: Proofs.

To save the data generated by the evaluation, run

    make -f tar.mk

Analysis
--------

To analyse the outcome of an evaluation, there are scripts in the
`json`,
`markdown`, and
`plot`
directories, generating the respective kind of data.
These scripts should be called from the `eval` directory, e.g.
`json/proofs.sh`.
Some of these scripts require [jq] or [jaq].

[jq]: https://stedolan.github.io/jq/
[jaq]: https://www.github.com/01mf02/jaq
