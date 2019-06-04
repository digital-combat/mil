:- module(mil_formations,
          [   formation_units/2         % ?Formation, ?Units
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

formation_units(Formation/Unit, [Unit|Units]) :-
    formation_units(Formation, Units),
    !.
formation_units(Unit, [Unit]).
