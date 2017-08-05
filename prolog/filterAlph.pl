/*************************************************************************

    File: filterAlph.pl
    Copyright (C) 2017, Michael White

    This file accompanies dycccg.pl, which makes use of BB1, and shares
    the same license.

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc.,
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(filterAlph,[filterAlphabeticVariants/2,number_list/2,number_list/3]).

:- use_module(alphaConversion,[alphabeticVariants/2]).
:- use_module(comsemPredicates,[selectFromList/3]).


/*========================================================================
   Filter Alphabetic Variants
========================================================================*/

filterAlphabeticVariants(L1,L2):-
	member((X,N1,S1),L1),
	selectFromList((Y,N2,[]),L1,L3), N1 < N2,
	alphabeticVariants(X,Y), !,
	append(S1,[N2],S3),
	subst((X,N1,S1),(X,N1,S3),L3,L4),
	filterAlphabeticVariants(L4,L2).

filterAlphabeticVariants(L,L).

subst(X,Y,[Z|T],[Y|T]) :- X == Z, !.
subst(X,Y,[Z|T],[Z|R]) :- subst(X,Y,T,R).


number_list([],[],_).
number_list([H|T],[(H,N,[])|R],N) :-
	N1 is N + 1,
	number_list(T,R,N1).

number_list(L,L2) :- number_list(L,L2,0).


