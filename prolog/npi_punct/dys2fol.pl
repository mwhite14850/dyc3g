/*************************************************************************

    File: dys2fol.pl
    Copyright (C) 2016 Michael White

    Based in part on drs2fol.pl in BB2, version 2.0
    by Patrick Blackburn & Johan Bos

    This code is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    The code is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB2; if not, write to the Free Software Foundation, Inc.,
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(dys2fol,[dys2fol/2]).


/*========================================================================
   Translate sentential DySs into FOL formulas
========================================================================*/

dys2fol(dys(Cond,[],[]),Formula):-
	cond2fol(Cond,Formula).

dys2fol(dys(Cond,[],[Cond2|Conds]),and(Formula,Formula2)):-
	cond2fol(Cond,Formula),
	conds2fol([Cond2|Conds],Formula2).

dys2fol(dys(Cond,[X|Referents],Conds),some(X,Formula)):-
	dys2fol(dys(Cond,Referents,Conds),Formula).

conds2fol([Cond],Formula) :-
	cond2fol(Cond,Formula).
conds2fol([Cond1,Cond2|Conds],and(Formula1,Formula2)) :-
	cond2fol(Cond1,Formula1),
	conds2fol([Cond2|Conds],Formula2).


/*========================================================================
   Translate DyS-Conditions into FOL formulas
========================================================================*/

cond2fol(not(DyS),not(Formula)):- !,
	dys2fol(DyS,Formula).

cond2fol(or(DyS1,DyS2),or(Formula1,Formula2)):- !,
	dys2fol(DyS1,Formula1),
	dys2fol(DyS2,Formula2).

cond2fol(imp(dys(Cond,[],Conds),DyS2),imp(Formula1,Formula2)):- !,
	dys2fol(dys(Cond,[],Conds),Formula1),
	dys2fol(DyS2,Formula2).

cond2fol(imp(dys(Cond,[X|Referents],Conds),DyS2),all(X,Formula)):- !,
	cond2fol(imp(dys(Cond,Referents,Conds),DyS2),Formula).

cond2fol(all(X,DySr,DySs),all(X,Formula)) :- !,
	cond2fol(imp(DySr,DySs),Formula).

cond2fol(most(X,DySr,DySs),most(X,FormulaR,FormulaS)) :- !,
	dys2fol(DySr,FormulaR), dys2fol(DySs,FormulaS).

cond2fol(few(X,DySr,DySs),few(X,FormulaR,FormulaS)) :- !,
	dys2fol(DySr,FormulaR), dys2fol(DySs,FormulaS).

cond2fol(and(Cond1,Cond2),and(Formula1,Formula2)) :- !,
	cond2fol(Cond1,Formula1), cond2fol(Cond2,Formula2).

cond2fol(Formula,Formula).


