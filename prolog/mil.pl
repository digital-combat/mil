:- module(mil, []).

:- multifile
    user:file_search_path/2.

user:file_search_path(mil, library(mil)).

:- public
    formation_property/2,
    rank_property/2,
    rank_expansion/2.

:- multifile
    formation_property/2,
    rank_property/2,
    rank_expansion/2.
