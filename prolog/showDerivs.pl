/*************************************************************************

    File: showDerivs.pl
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

:- module(showDerivs,[derivs/0,noderivs/0,showDerivList/1]).


/*========================================================================
   Show Derivations
========================================================================*/

:- dynamic show_derivs/0.

% turn derivs on/off
derivs :- assert(show_derivs).
noderivs :- retract(show_derivs).

showDerivList(_) :- \+ show_derivs, !.
showDerivList(Signs) :- showDerivList(Signs,1).

showDerivList([],_).
showDerivList([Sign|Tail],N) :-
	nl, print('Derivation '), print(N), nl, nl,
	showDeriv(Sign,0), nl,
	N1 is N+1,
	showDerivList(Tail,N1).

showDeriv(sign(W,C,D),Tab) :-
	D = deriv(ChildSigns,Op),
	tab(Tab),
	Tab1 is Tab + 1, Tab2 is Tab1 + 1,
	atomics_to_string(W,' ',Str),
	print(Str), print(' :- '),
	nl, tab(Tab2),
	C=cat(Syn,Sem,Type),
	printCat(Syn), %print(' : '),
	nl, tab(Tab2),
	\+ \+ (numbervars(Sem,0,_), print(Sem)),
	nl, tab(Tab2),
	\+ \+ (numbervars(Type,0,_), printType(Type)),
	nl, tab(Tab2),
	atomics_to_string(Op,'_',OpStr),
	print('('), print(OpStr), print(')'),
	nl,
	showDerivs(ChildSigns,Tab1).

showDerivs([],_).
showDerivs([Sign|Rest],Tab) :-
	showDeriv(Sign,Tab),
	showDerivs(Rest,Tab).

printCat(V) :- var(V), !, print(V).
printCat(fslash(A,B)) :- !,
	printChildCat(A), write('/'), printChildCat(B).
printCat(bslash(A,B)) :- !,
	printChildCat(A), write('\\'), printChildCat(B).
printCat(tower(A,B,C)) :- !,
	printChildCat(A), write(' _|_ '), printChildCat(B),
	write(' __||__ '), printChildCat(C).
printCat(delim(A)) :- !,
	write('<'), printChildCat(A), write('>').
printCat(AtomCat) :- print(AtomCat).

printChildCat(Cat) :-
	is_complex(Cat), !,
	print('('), printCat(Cat), print(')').
printChildCat(Cat) :- printCat(Cat).

is_complex(V) :- var(V), !, fail.
is_complex(fslash(_,_)).
is_complex(bslash(_,_)).
is_complex(tower(_,_,_)).

printType(T) :- var(T), !, print(T).
printType(T) :- atom(T), !, print(T).
printType(m(T)) :- \+var(T), T=to(_,_), !,
	write('M('), printType(T), write(')').
printType(m(T)) :- !, write('M'), printType(T).
printType(to(A,B)) :- \+ var(A), A=to(_,_), !,
	write('('), printType(A), write(')'), write(' -> '), printType(B).
printType(to(A,B)) :- !, printType(A), write(' -> '), printType(B).
printType(T) :- print(T).

