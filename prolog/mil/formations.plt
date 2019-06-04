:- begin_tests(mil_formations).

:- use_module(formations).

test(formation_units, [true(A==[c, b, a])]) :-
    formation_units(a/b/c, A).

test(formation_units, [true(A==1/2/3)]) :-
    formation_units(A, [3, 2, 1]).

test(formation_units, [true(A-B==1/2-3), nondet]) :-
    formation_units(A/B, [3, 2, 1]).

:- end_tests(mil_formations).
