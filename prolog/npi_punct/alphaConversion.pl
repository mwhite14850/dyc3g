/*************************************************************************

    File: alphaConversion.pl
    Copyright (C) 2004,2005,2006 Patrick Blackburn & Johan Bos

    Modifications copyright (C) 2016, Michael White
    Modified to also convert dynamic semantic structures

    This file is part of BB1, version 1.3 (November 2006).

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

:- module(alphaConversion,[alphaConvert/2,
                           alphabeticVariants/2]).

:- use_module(comsemPredicates,[compose/3,
				memberList/2]).


/*========================================================================
   Alpha Conversion (introducing substitutions)
========================================================================*/

% TODO: consider extending to normalize order of conjunction
alphaConvert(F1,F2):-
   alphaConvert(F1,[],[]-_,F2).


/*========================================================================
   Alpha Conversion
========================================================================*/

alphaConvert(X,Sub,Free1-Free2,Y):-
   var(X),
   (
      memberList(sub(Z,Y),Sub),
      X==Z, !,
      Free2=Free1
   ;
      Y=X,
      Free2=[X|Free1]
   ).

alphaConvert(Expression,Sub,Free1-Free2,some(Y,F2)):-
   nonvar(Expression),
   Expression = some(X,F1),
   alphaConvert(F1,[sub(X,Y)|Sub],Free1-Free2,F2).

alphaConvert(Expression,Sub,Free1-Free2,all(Y,F2)):-
   nonvar(Expression),
   Expression = all(X,F1),
   alphaConvert(F1,[sub(X,Y)|Sub],Free1-Free2,F2).

alphaConvert(Expression,Sub,Free1-Free2,lam(Y,F2)):-
   nonvar(Expression),
   Expression = lam(X,F1),
   alphaConvert(F1,[sub(X,Y)|Sub],Free1-Free2,F2).

alphaConvert(Expression,Sub,Free1-Free3,que(Y,F3,F4)):-
   nonvar(Expression),
   Expression = que(X,F1,F2),
   alphaConvert(F1,[sub(X,Y)|Sub],Free1-Free2,F3),
   alphaConvert(F2,[sub(X,Y)|Sub],Free2-Free3,F4).

% mww: added dys
alphaConvert(Expression,Sub,Free1-Free3,dys(E2,[],Conds2)):-
   nonvar(Expression),
   Expression = dys(E1,[],Conds1),
   alphaConvert(E1,Sub,Free1-Free2,E2),
   alphaConvertList(Conds1,Sub,Free2-Free3,Conds2).

alphaConvert(Expression,Sub,Free1-Free2,dys(E2,[Y|Ys],Conds2)):-
   nonvar(Expression),
   Expression = dys(E1,[X|Xs],Conds1),
   (   var(X)
   ->  Sub1 = [sub(X,Y)|Sub]
   ;   (Sub1 = Sub, X = Y) ),
   alphaConvert(dys(E1,Xs,Conds1),Sub1,Free1-Free2,dys(E2,Ys,Conds2)).

% mww: added all/3, most/3, few/3
alphaConvert(Expression,Sub,Free1-Free3,all(Y,F3,F4)):-
   nonvar(Expression),
   Expression = all(X,F1,F2),
   alphaConvert(F1,[sub(X,Y)|Sub],Free1-Free2,F3),
   alphaConvert(F2,[sub(X,Y)|Sub],Free2-Free3,F4).

alphaConvert(Expression,Sub,Free1-Free3,most(Y,F3,F4)):-
   nonvar(Expression),
   Expression = most(X,F1,F2),
   alphaConvert(F1,[sub(X,Y)|Sub],Free1-Free2,F3),
   alphaConvert(F2,[sub(X,Y)|Sub],Free2-Free3,F4).

alphaConvert(Expression,Sub,Free1-Free3,few(Y,F3,F4)):-
   nonvar(Expression),
   Expression = few(X,F1,F2),
   alphaConvert(F1,[sub(X,Y)|Sub],Free1-Free2,F3),
   alphaConvert(F2,[sub(X,Y)|Sub],Free2-Free3,F4).

alphaConvert(F1,Sub,Free1-Free2,F2):-
   nonvar(F1),
   \+ F1 = some(_,_),
   \+ F1 = all(_,_),
   \+ F1 = lam(_,_),
   \+ F1 = que(_,_,_),
   \+ F1 = dys(_,_,_),
   \+ F1 = all(_,_,_),
   \+ F1 = most(_,_,_),
   \+ F1 = few(_,_,_),
   compose(F1,Symbol,Args1),
   alphaConvertList(Args1,Sub,Free1-Free2,Args2),
   compose(F2,Symbol,Args2).


/*========================================================================
   Alpha Conversion (listwise)
========================================================================*/

alphaConvertList([],_,Free-Free,[]).

alphaConvertList([X|L1],Sub,Free1-Free3,[Y|L2]):-
   alphaConvert(X,Sub,Free1-Free2,Y),
   alphaConvertList(L1,Sub,Free2-Free3,L2).


/*========================================================================
   Alphabetic Variants
========================================================================*/

alphabeticVariants(Term1,Term2):-
   alphaConvert(Term1,[],[]-Free1,Term3),
   alphaConvert(Term2,[],[]-Free2,Term4),
   Free1==Free2,
   numbervars(Free1,0,N),
   numbervars(Term3,N,M),
   numbervars(Term4,N,M),
   Term3=Term4.
