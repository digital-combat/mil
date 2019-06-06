:- module(mil_formations,
          [   formation_units/2,        % ?Formation, ?Units
              formation_concat/3        % ?Minor, ?Major, ?Formation
          ]).

%!  formation_units(?Formation, ?Units:list) is semidet.
%
%   Unifies Formation with its component Units. Formation has minor
%   ordering; Units have major ordering.
%
%   Examples as follows.
%
%       ?- formation_units(A/B/C, Units).
%       Units = [C, B, A].
%
%   Units are formation sub-components, not necessarily individual
%   elements. What is an element? A tank, a ship, a soldier. Elements
%   have no sub-units. Formation is a full or partial formation
%   designation.
%
%   Unifying in mode (-, +) when Formation has fewer sub-units that
%   Units matches the major sub-units first. So, for instance, in clause
%   formation_units(Minor/Major, [a, b, c]) unifies Minor with =c/b=.
%
%   Variables bind a compound. Hence a Formation argument of A unifies
%   with compound argument B/C by default. Stop this happening.
%
%   @arg Formation is a slash-delimited compound describing the
%   formation in ascending order, small to large sub-unit; that is
%   minor-priority ordering.
%
%   @arg Units is a list of sub-units in descending, large to
%   small, major-priority order.

formation_units(Unit, [Unit]) :-
    \+ compound(Unit),
    !.
formation_units(Formation/Unit, [Unit|Units]) :-
    formation_units(Formation, Units).

%!  formation_concat(?Minor, ?Major, ?Formation) is semidet.
%
%   Acts as an append/3 method but for formations. The first compound
%   unifies with the minor components; the second with the major.
%
%   Works by first reducing Minor and Major to major-priority unit
%   lists, prior to concatenation.

formation_concat(Minor, Major, Formation) :-
    formation_units(Minor, Minors),
    formation_units(Major, Majors),
    once(append(Majors, Minors, Units)),
    formation_units(Formation, Units).
