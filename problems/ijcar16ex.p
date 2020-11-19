% This problem corresponds to the formula F# given in Section 2 of the article
% "nanoCoP: A Non-clausal Connection Prover" by Jens Otten,
% published at IJCAR 2016.
fof(1, conjecture, (
  ( p(a)
  & ( ( (q(f(f(c))) & ![X]: (q(f(X)) => q(X)))
      & ~q(c)
      )
    | ![Y]: (p(Y) => p(g(Y)))
    )
  ) => ?[Z]: p(g(g(Z))))).

% (~((q(f(f(c))) & ![X]: (q(f(X)) => q(X))) => q(c))))).

