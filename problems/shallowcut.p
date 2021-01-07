% This problem demonstrates the difference between
% no cut, shallow cut, and deep cut:
% Deep cut is not able to solve the problem,
% and shallow cut takes fewer inferences than no cut.
fof(start, negated_conjecture, ![X]: (p(X) | q(X))).
fof(dead_end, axiom, ![X]: (~p(X) | r(X))).
fof(dead_end1, axiom, ~r(a)).
fof(dead_end2, axiom, ~r(a) | $false).
fof(rescue, axiom, ![X]: ~p(X)).
fof(salvation, axiom, ~q(b)).
