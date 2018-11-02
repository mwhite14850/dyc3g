/*************************************************************************

    File: rules.pl
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

:- module(rules,[combine/3,combine/4]).

:- use_module(monadic,[lower/2,undelimit/4,delimit/3,
		       monadic_base_arg/1,monadic_lift/2]).
:- use_module(grammar,[typeraise/3]).


/*========================================================================
   Base Binary Rules

   TODO: other combinatory rules, other eta coercion cases;
         sensible coordination beyond one-level towers
========================================================================*/

% forward application
%
% X/Y : F : T1 -> T2        Y : A : T1
% ------------------------------------
%             X: (F A): T2
combine(cat(fslash(X,Y),F,to(T1,T2)),cat(Y,A,T1),cat(X,app(F,A),T2),['>']).

% forward application with eta coercion
combine(cat(fslash(X,Y),F,to(m(T1),T2)),
	cat(Y,A,T1),
	cat(X,app(F,dys(A,[],[])),T2),
	['EtaR','>']).

% forward application with rightward monadic lift, for coordination
combine(cat(fslash(X,tower(s(P1,B1,E1),s(P2,B2,E2),Y)),F,to(T1,T2)),
	cat(Z,A,T),
	cat(X,app(F,RA),T2),
	['MLiftR','>']) :-

	Z \= tower(_,_,_), % only use monadic lift where necessary
	\+ var(A),         % needed to block infinite regress with preceding LiftR
	monadic_lift(cat(Z,A,T),cat(tower(s(P1,B1,E1),s(P2,B2,E2),Y),RA,T1)).


% backward application
combine(cat(Y,A,T1),cat(bslash(X,Y),F,to(T1,T2)),cat(X,app(F,A),T2),['<']).

% backward application with eta coercion
combine(cat(Y,A,T1),
	cat(bslash(X,Y),F,to(m(T1),T2)),
	cat(X,app(F,dys(A,[],[])),T2),
	['EtaR','<']).

% backward application with leftward monadic lift, for coordination
combine(cat(Z,A,T),
	cat(bslash(X,tower(s(P1,B1,E1),s(P2,B2,E2),Y)),F,to(T1,T2)),
	cat(X,app(F,RA),T2),
	['MLiftL','<']) :-

	Z \= tower(_,_,_), % only use monadic lift where necessary
	\+ var(A),         % needed to block infinite regress with preceding LiftL
	monadic_lift(cat(Z,A,T),cat(tower(s(P1,B1,E1),s(P2,B2,E2),Y),RA,T1)).


% forward composition
combine(cat(fslash(X,Y),F,to(T2,T3)),
	cat(fslash(Y,Z),G,to(T1,T2)),
	cat(fslash(X,Z),lam(U,app(F,app(G,U))),to(T1,T3)),
	['>B']).


% backward crossed composition
combine(cat(fslash(Y,Z),G,to(T1,T2)),
	cat(bslash(X,Y),F,to(T2,T3)),
	cat(fslash(X,Z),lam(U,app(F,app(G,U))),to(T1,T3)),
	['<Bx']).


/*========================================================================
   Tower Binary Rules
   TODO: lower left
========================================================================*/

% delimit left
combine(cat(X,A,T1),cat(T,F,T2),cat(C,Res,T4),Op) :-
	undelimit(T,U,backward,ArgY), !,
	delimit(cat(X,A,T1),ArgY,cat(R,L,T3)),
	combine(cat(R,L,T3),cat(U,F,T2),cat(C,Res,T4),Op2),
	Op = ['DelimL'|Op2].

% delimit right
combine(cat(T,F,T1),cat(X,A,T2),cat(C,Res,T4),Op) :-
	undelimit(T,U,forward,ArgY), !,
	delimit(cat(X,A,T2),ArgY,cat(R,L,T3)),
	combine(cat(U,F,T1),cat(R,L,T3),cat(C,Res,T4),Op2),
	Op = ['DelimR'|Op2].

% lower right
combine(cat(A,M,T1),
	cat(tower(E,F,B),N,T2),
	cat(C,Res,T4),
	Op)
:-
	monadic_base_arg(T1),
	lower(cat(tower(E,F,B),N,T2),cat(L,N0,T3)),
	combine(cat(A,M,T1),
		cat(L,N0,T3),
		cat(C,Res,T4),
		BotOp),
	Op = ['LowerR'|BotOp].

% combination
combine(cat(tower(D,E,A),M,to(to(T1,Alpha),Beta)),
	cat(tower(E,F,B),N,to(to(T2,Gamma),Alpha)),
	cat(tower(D,F,C),Res,to(to(T,Gamma),Beta)),
	Op)
:-
	combine(cat(A,X,T1),cat(B,Y,T2),cat(C,Z,T),BotOp),
	Op = ['Comb'|BotOp],
	Res = lam(K,app(M,lam(X,app(N,lam(Y,app(K,Z)))))).

% lift left
combine(cat(A,M0,T1),
	cat(tower(D,E,B),N,to(to(T2,Alpha),Beta)),
	cat(tower(D,E,C),Res,to(to(T3,Alpha),Beta)),
	Op)
:-
	combine(cat(A,X,T1),cat(B,Y,T2),cat(C,Z,T3),BotOp),
	Op = ['LiftL'|BotOp],
	M = lam(P,app(P,M0)),
	Res = lam(K,app(M,lam(X,app(N,lam(Y,app(K,Z)))))).

% lift right
% nb: need to check that left tower has balanced punct
combine(cat(tower(D,E,A),M,to(to(T1,Alpha),Beta)),
	cat(B,N0,T2),
	cat(tower(D,E2,C),Res,to(to(T3,Alpha),Beta)),
	Op)
:-
	E = s(Pol,bal,_), E2 = s(Pol,_,_),
	combine(cat(A,X,T1),cat(B,Y,T2),cat(C,Z,T3),BotOp),
	Op = ['LiftR'|BotOp],
	N = lam(P,app(P,N0)),
	Res = lam(K,app(M,lam(X,app(N,lam(Y,app(K,Z)))))).



/*========================================================================
   Tower and Base Unary Rules
========================================================================*/

% combine single tower
combine(cat(tower(A,B,C),Sem,to(to(T1,Alpha),Beta)),
	cat(tower(A,B,D),Res,to(to(T2,Alpha),Beta)),
	Op)
:-
	combine(cat(C,X,T1),cat(D,F,T2),Op),
	Res=lam(K,app(Sem,lam(X,app(K,F)))).

% type raising
combine(Cat1,Cat,Op) :-
	typeraise(Cat1,Cat,Op).



