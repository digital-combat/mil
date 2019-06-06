:- module(mil_unit,
          [   alpha_ordinal//1,         % ?Unit
              alpha//1,                 % ?Alpha
              ordinal//1                % ?Ordinal
          ]).

%!  alpha_ordinal(?Unit:atom)// is nondet.
%
%   Alpha-ordinal phrases for a military sub-unit. Unit is =a1= through
%   =z99=, and the corresponding codes run from `A1` through `Z99`. Used
%   for company-battalion sub-units, e.g. =a1= for alfa company, first
%   battalion; also used for platoon sections, e.g. =b2= for bravo
%   section, second platoon.

alpha_ordinal(Unit) -->
    alpha(Alpha),
    ordinal(Ordinal0),
    {   atom_number(Ordinal, Ordinal0),
        atom_concat(Alpha, Ordinal, Unit)
    }.

%!  alpha(?Alpha:atom)// is nondet.
%
%   Unifies atomic formation unit =a= through =z= with its upper-case
%   codes, actually just one code.

alpha(Alpha) -->
    {   code_type(Code, upper(Alpha0)),
        code_type(Code, ascii),
        atom_codes(Alpha, [Alpha0])
    },
    [Code].

%!  ordinal(?Ordinal:nonneg)// is nondet.
%
%   Limits ordinals to two decimal digits. Phrase integer//1 does not
%   work in (?, ?, ?) mode.

ordinal(Ordinal) -->
    {   between(1, 99, Ordinal),
        number_codes(Ordinal, Codes)
    },
    Codes.
