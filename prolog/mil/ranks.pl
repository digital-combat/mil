:- module(mil_ranks, [rank_property/2]).

:- use_module(library(mil)).

:- multifile
    mil:property_of_rank/2.

%!  rank_property(?Rank, ?Property) is nondet.
%
%   Rank Property terms include:
%
%       * defined
%       * grade(?Grade:atom)
%       * level(?Level:nonneg)
%       * prime(?Prime:atom)
%       * order(?Order:number)
%
%       Order is a number, nominally between 0 and 40, representing the
%       Rank ordering somewhere in-between the lowest enlisted rank and
%       the highest commissioned rank. Elements with higher rank take
%       authority over others with lower rank, assuming they belong to
%       the same unit.
%
%       * super(?Super)
%
%       Unifies Rank with sub-ordinate ranks.
%
%   Does not perform rank expansion. Instead presumes Rank already
%   fully expanded with grade, level and prime components.

rank_property(Grade-Level/Prime, defined) :-
    grade_order(Grade, _),
    level_order(Level, _),
    prime_order(Prime, _).
rank_property(Grade-Level/Prime, grade(Grade)) :-
    rank_property(Grade-Level/Prime, defined).
rank_property(Grade-Level/Prime, level(Level)) :-
    rank_property(Grade-Level/Prime, defined).
rank_property(Grade-Level/Prime, prime(Prime)) :-
    rank_property(Grade-Level/Prime, defined).
rank_property(Grade-Level/Prime, order(Order)) :-
    grade_order(Grade, Grade0),
    level_order(Level, Level0),
    prime_order(Prime, Prime0),
    Order is Grade0 + Level0 + Prime0.
rank_property(Rank, super(Super)) :-
    rank_property(Rank, order(Order0)),
    rank_property(Super, order(Order)),
    Order0 @< Order.
rank_property(Rank, Property) :-
    mil:property_of_rank(Property, Rank).

%!  grade_order(?Grade:atom, ?Order:number) is nondet.
%
%   There are three tiers:
%
%       - commissioned officer
%       - non-commissioned officer
%       - enlisted
%
%   Non-commissioned officer grades subsume warrant officers, although
%   some warrant officers received commissioned status once upon a time
%   in the Royal Navy.

grade_order(enlisted,                   0).
grade_order(noncommissioned,            10).
grade_order(commissioned,               20).

%!  level_order(?Level:nonneg, ?Order:number) is nondet.
%
%   Order for Level. The Level cannot be a compound, or an atom. It must
%   be a variable or an integer, nothing more. Fails if not.

level_order(Level, Level) :-
    level(Level),
    between(1, 9, Level).

level(Level) :-
    var(Level),
    !.
level(Level) :-
    integer(Level).

%!  prime_order(?Prime:atom, ?Order:number) is nondet.
%
%   Prime is either =a= or =b=. The corresponding Order demotes bravo.
%   The prime does not boost one level to the next level of rank.

prime_order(a,                          0).
prime_order(b,                          -0.5).
