% This problem demonstrates the difference between
% no cut, exclusive (EX), and inclusive cut (EI) on extension steps:
% inclusive cut is not able to solve the problem, and
% exclusive cut takes fewer inferences than no cut.
% (Assuming conjecture-directed search is activated.)
fof(start, conjecture, ~(![X]: (p(X) | q(X)))).
fof(dead_end, axiom, ![X]: (~p(X) | r(X))).
fof(dead_end1, axiom, ~r(a)).
fof(dead_end2, axiom, (~r(a) | $false)).
fof(rescue, axiom, ![X]: ~p(X)).
fof(salvation, axiom, ~q(b)).
