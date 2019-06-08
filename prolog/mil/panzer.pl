:- module(mil_panzer, [kpfw_number//1]).

%!  kpfw_number(?Formation) is nondet.
%
%   Composes or decomposes Panzer fighting vehicle numbers. Formation
%   sub-units unify with digits in one of two formats: KZW or KKZW,
%   where K or KK indicate company, Z for platoon or troop, and W
%   for vehicle element within troop. Limits the number of companies per
%   regiment to 99.

kpfw_number(Element/Platoon/Company) -->
    kpfw_number(1-99, Company),
    kpfw_number(0-9, Platoon),
    kpfw_number(1-9, Element).

kpfw_number(From-To, Unit) -->
    {   between(From, To, Unit),
        number_codes(Unit, Codes)
    },
    Codes.
