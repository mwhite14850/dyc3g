/*************************************************************************

    File: reduce.pl
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

:- module(reduce,[reduce/2,reduce_sign/2,seq_reduce/2]).

:- use_module(betaConversion,[betaConvert/2]).


/*========================================================================
   Beta and Sequence Reduction
========================================================================*/

% reduce sem in sign
reduce_sign(sign(W,cat(Syn,Sem,Type),D),sign(W,cat(Syn,SemR,Type),D)) :-
	reduce(Sem,SemR).

% beta and sequence reduce
reduce(Sem,SemR) :-
	betaConvert(Sem,SemBR), seq_reduce(SemBR,SemR).

% sequence reduce
% nb: also reduces seq_st, adding semantics of second item to conditions
seq_reduce(V,V) :- var(V), !.
seq_reduce(seq(A,lam(Y,dys(V,[],[]))),T) :-
	Y == V, !,
	seq_reduce(A,T).
seq_reduce(seq(A,lam(Y,B)),dys(T,DRs,Conds)) :-
	seq_reduce(A,Ar), seq_reduce(B,Br),
	Ar=dys(X,DRs1,Conds1),
	betaConvert(app(lam(Y,Br),X),dys(T,DRs2,Conds2)), !,
	append(DRs1,DRs2,DRs), append(Conds1,Conds2,Conds).
seq_reduce(seq(A,lam(Y,B)),BetaRed) :-
	seq_reduce(A,Ar), seq_reduce(B,Br),
	Ar=dys(X,[],[]),
	betaConvert(app(lam(Y,Br),X),BetaRed), !.
seq_reduce(seq(A,lam(Y,B)),seq(Ar,lam(Y,Br))) :-
	seq_reduce(A,Ar), seq_reduce(B,Br), !.
seq_reduce(seq_st(A,lam(Y,B)),dys(X,DRs,Conds)) :-
	seq_reduce(A,Ar), seq_reduce(B,Br),
	Ar=dys(X,DRs1,Conds1),
	betaConvert(app(lam(Y,Br),X),dys(T,DRs2,Conds2)), !,
	append(DRs1,DRs2,DRs),
	reduce_and(T,CondsT), append(CondsT,Conds2,CondsT2),
	append(Conds1,CondsT2,Conds).
seq_reduce(dys(V,DRs,Conds1),dys(V,DRs,Conds2)) :-
	var(V), !, seq_reduce_list(Conds1,Conds2).
seq_reduce(dys(not(DyS1),DRs,Conds1),dys(not(DyS2),DRs,Conds2)) :- !,
	seq_reduce(DyS1,DyS2), seq_reduce_list(Conds1,Conds2).
seq_reduce(dys(imp(DySa1,DySc1),DRs,Conds1),dys(imp(DySa2,DySc2),DRs,Conds2)) :- !,
	seq_reduce(DySa1,DySa2), seq_reduce(DySc1,DySc2),
	seq_reduce_list(Conds1,Conds2).
seq_reduce(dys(all(X,DySr1,DySs1),DRs,Conds1),dys(all(X,DySr2,DySs2),DRs,Conds2)) :- !,
	seq_reduce(DySr1,DySr2), seq_reduce(DySs1,DySs2),
	seq_reduce_list(Conds1,Conds2).
seq_reduce(dys(most(X,DySr1,DySs1),DRs,Conds1),dys(most(X,DySr2,DySs2),DRs,Conds2)) :- !,
	seq_reduce(DySr1,DySr2), seq_reduce(DySs1,DySs2),
	seq_reduce_list(Conds1,Conds2).
seq_reduce(dys(few(X,DySr1,DySs1),DRs,Conds1),dys(few(X,DySr2,DySs2),DRs,Conds2)) :- !,
	seq_reduce(DySr1,DySr2), seq_reduce(DySs1,DySs2),
	seq_reduce_list(Conds1,Conds2).
seq_reduce(dys(E1,DRs,Conds1),dys(E2,DRs,Conds2)) :- !,
	E1 =.. [Functor|Args1],
	seq_reduce_list(Args1,Args2),
	E2 =.. [Functor|Args2],
	seq_reduce_list(Conds1,Conds2).
seq_reduce(lam(X,T1),lam(X,T2)) :- !,
	seq_reduce(T1,T2).
seq_reduce(T,T).

seq_reduce_list([],[]).
seq_reduce_list([V|Tail1],[V|Tail2]) :-
	var(V), !, seq_reduce_list(Tail1,Tail2).
seq_reduce_list([not(DyS1)|Tail1],[not(DyS2)|Tail2]) :- !,
	seq_reduce(DyS1,DyS2),
	seq_reduce_list(Tail1,Tail2).
seq_reduce_list([imp(DySa1,DySc1)|Tail1],[imp(DySa2,DySc2)|Tail2]) :- !,
	seq_reduce(DySa1,DySa2), seq_reduce(DySc1,DySc2),
	seq_reduce_list(Tail1,Tail2).
seq_reduce_list([all(X,DySr1,DySs1)|Tail1],[all(X,DySr2,DySs2)|Tail2]) :- !,
	seq_reduce(DySr1,DySr2), seq_reduce(DySs1,DySs2),
	seq_reduce_list(Tail1,Tail2).
seq_reduce_list([most(X,DySr1,DySs1)|Tail1],[most(X,DySr2,DySs2)|Tail2]) :- !,
	seq_reduce(DySr1,DySr2), seq_reduce(DySs1,DySs2),
	seq_reduce_list(Tail1,Tail2).
seq_reduce_list([few(X,DySr1,DySs1)|Tail1],[few(X,DySr2,DySs2)|Tail2]) :- !,
	seq_reduce(DySr1,DySr2), seq_reduce(DySs1,DySs2),
	seq_reduce_list(Tail1,Tail2).
seq_reduce_list([Cond|Tail1],[Cond|Tail2]) :-
	seq_reduce_list(Tail1,Tail2).

% reduce conjoined conditions to a list
reduce_and(and(A,B),List) :- !,
	reduce_and(A,ListA), reduce_and(B,ListB),
	append(ListA,ListB,List).
reduce_and(A,[A]).


