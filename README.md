# Connection Provers

This project aims to reimplement in Rust
several automated theorem provers of the leanCoP family,
such as [leanCoP] and [nanoCoP].

The goals of the project are:

* Performance:
  The provers should perform as many inferences per time as possible.
* Compatibility with the original provers:
  The provers should be able to reproduce
  the same steps as the original provers if so desired.
  This is to help debugging and to guarantee soundness and completeness.

Currently, the only implemented prover is *meanCoP* based on leanCoP.

To run the prover, it is necessary to install a recent Rust toolchain.
This can be done conveniently using [rustup].

Once Rust is installed, you can run meanCoP such as:

    cargo run --release problems/ijcar16ex.p

To install meanCoP:

    cargo install --path meancop

The project contains an extensive evaluation suite in the `eval` directory.

[leanCoP]: http://leancop.de/
[nanoCoP]: http://leancop.de/nanocop/
[rustup]: https://rustup.rs/
