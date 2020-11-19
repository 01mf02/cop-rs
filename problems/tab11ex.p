% This problem corresponds to the formula F1 given in Section 2 of the article
% "A Non-clausal Connection Calculus" by Jens Otten,
% published at TABLEAUX 2011.
fof(1, conjecture, ((![X]: ((~p(X) | q(f(X))) => (q(X) & (q(a) => r(b)) & ~r(X))) & q(f(b))) => p(a))).
