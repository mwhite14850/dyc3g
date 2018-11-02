/*************************************************************************

    File: monadic.pl
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

:- module(monadic,[lower_result_tower/4,lower_if_poss/2,lower/2,
		   undelimit/4,delimit/3,monadic_base_arg/1,monadic_lift/2,
		   neg_cat/1]).

:- use_module(reduce,[reduce/2]).


/*========================================================================
   lower result
========================================================================*/

% lower cat with tower result if possible
lower_result_tower(Cat0,Cat,Op0,Op) :-
	Cat0=cat(T,_,_), has_tower_result(T),
	lower(Cat0,Cat), !,
	Op = ['Lower'|Op0].

lower_result_tower(Cat,Cat,Op,Op).

has_tower_result(tower(tower(_,_,_),_,_)) :- !.
has_tower_result(tower(_,_,T)) :-
    has_tower_result(T).


/*========================================================================
   lower if possible
========================================================================*/

lower_if_poss(sign(W,Cat1,D1),sign(W,Cat,D)) :-
	lower(Cat1,Cat), !,
	D = deriv([sign(W,Cat1,D1)],['Lower']).
lower_if_poss(Sign,Sign).


/*========================================================================
   Recursively lower: recurse through tower(s,s,A) down to
     either tower(B,s,s), with result B,
     or to tower(s,s,C), where C is s\np or s/np, with result C
     nb: this doesn't handle the case with a tower result on
         the top level with a level inserted in the middle
========================================================================*/

% base case: single-level tower(B,s,s), apply eta continuation
% (t -> Mt) -> alpha => alpha
lower(cat(tower(B,s(_,Bal,End),s(+,Bal,End)),K,to(to(t,m(t)),Alpha)),
      cat(B2,app(K,lam(X,dys(X,[],[]))),Alpha))
:-
	\+ neg_cat_syn(B), prop_punct_info(B,Bal,End,B2).

% single-level tower with Mt on bottom
% apply identity continuation, short-circuiting mult-step
% derivation of this in Fact A.12 in Charlow (2014)
% (Mt -> Mt) -> Mt => Mt
lower(cat(tower(s(+,_,_),s(_,Bal,End),s(+,Bal,End)),K,to(to(m(t),m(t)),m(t))),
      cat(s(+,Bal,End),app(K,lam(X,X)),m(t))).


% single-level tower(s,s,C), with C = s\np or s/np
% apply continuation over X and abstract again over X
% ((e -> t) -> Mt) -> Mt => e -> Mt
lower(cat(tower(s(+,_,_),s(_,Bal,End),C),K,to(to(to(e,t),m(t)),m(t))),
      cat(C,lam(X,app(K,lam(P,dys(app(P,X),[],[])))),to(e,m(t))))
:-
	( C = bslash(s(+,Bal,End),Y) ; C = fslash(s(+,Bal,End),Y) ),
	atom(Y).


% recursive case: construct lowered semantics L from continuation K for
% bottom of tower, then apply input semantics to lambda K . L to
% to achieve one swell foop lowering; note that for a three-level tower,
% lambda K . L will be lambda K . K eta; this is equivalent to reducing
% lam k . g (k a) to (g c) if A:a => C:c
% (nb: for selective exceptional scope-taking, need to use eta here instead)

% simpler case: A doesn't have a tower result
lower(cat(tower(s(+,_,_),s(_,Bal,End),A),Sem,to(to(T,m(t)),m(t))),cat(B,Lowered,m(t))) :-
	A = tower(R,_,_), R \= tower(_,_,_),
	lower(cat(A,K,T),cat(B,L,m(t))),
	Lowered=app(Sem,lam(K,L)),
	punct_info(B,Bal,End).

% A has a tower result: need to move lam K2 to the top so
% the result is a continuation, also need different return type;
% this is equivalent to reducing lam k . g (k a) to lam k . g (c k)
% if A:a => C:c
lower(cat(tower(s(+,_,_),s(_,Bal,End),A),Sem,to(to(Beta,Alpha),Alpha)),
      cat(B,Lowered,to(Gamma,Alpha))) :-
        A = tower(tower(_,s(_,Bal,End),_),_,_),
        lower(cat(A,K,Beta),cat(B,L,to(Gamma,Alpha))),
        Lowered=lam(K2,app(Sem,lam(K1,app(app(lam(K,L),K1),K2)))),
	punct_info(B,Bal,End).


/*========================================================================
   Undelimit: remove delimit from forward or backward arg
========================================================================*/

undelimit(fslash(X,delim(Y)),fslash(X,Y),forward,Y).
undelimit(bslash(X,delim(Y)),bslash(X,Y),backward,Y).
undelimit(tower(A,B,C),tower(A,B,D),Dir,Arg) :-
	undelimit(C,D,Dir,Arg).


/*========================================================================
   Delimit: Reset tower by lowering, monadic lifting
========================================================================*/

% leave unlifted (non-tower) cats as is
delimit(cat(C,Sem,Type),_,cat(C,Sem,Type)) :-
	\+ C=tower(_,_,_), !.

% tower case
delimit(cat(T,Sem,T1),ArgY,cat(R,Lifted,T3)) :-
	T = tower(_,_,_),
	lower(cat(T,Sem,T1),cat(ArgY,Lowered,T2)),
	reduce(Lowered,Reduced),
	monadic_lift(cat(ArgY,Reduced,T2),cat(R,Lifted,T3)).


/*========================================================================
   Check for monadic base arg
========================================================================*/

% base type with monadic first arg
monadic_base_arg(to(m(_),_)).

% tower type: recurse
monadic_base_arg(to(to(T,m(_)),m(_))) :-
	monadic_base_arg(T).


/*========================================================================
   Monadic lift: lift cat into underlying monad (if nec.)
   nb: preceding tower required to have balanced punct
========================================================================*/

% identity for already continuized cats
monadic_lift(cat(tower(A,B,C),Sem,Type),cat(tower(A,B,C),Sem,Type)) :- !.

% identity for already continuized cats as return types
monadic_lift(cat(fslash(tower(A,B,C),D),Sem,Type),
	     cat(fslash(tower(A,B,C),D),Sem,Type)) :- !.
monadic_lift(cat(fslash(bslash(tower(A,B,C),D),E),Sem,Type),
	     cat(fslash(bslash(tower(A,B,C),D),E),Sem,Type)) :- !.

% treat cats with injected values like values (per below)
% { <E,_> } => lam K . K E : (beta -> M alpha) -> M alpha
monadic_lift(cat(C,dys(E,[],[]),m(Beta)),
	     cat(tower(s(Pol,bal,_),s(Pol,Bal,End),C),
		 lam(K,app(K,E)),
		 to(to(Beta,m(Alpha)),m(Alpha)))) :- !,
	punct_info(C,Bal,End).

% sequence cats in the underlying monad
% M alpha => (alpha -> M beta) -> M beta
monadic_lift(cat(C,dys(E,DRs,Conds),m(Alpha)),
	     cat(tower(s(Pol,bal,_),s(Pol,Bal,End),C),
		 lam(K,seq(dys(E,DRs,Conds),lam(X,app(K,X)))),
		 to(to(Alpha,m(Beta)),m(Beta)))) :- !,
	punct_info(C,Bal,End).

% sequence cats with abstractions over the underlying monad
% alpha -> M beta => ((alpha -> beta) -> M gamma) -> M gamma
monadic_lift(cat(C,lam(Y,DyS),to(Alpha,m(Beta))),
	     cat(tower(s(Pol,bal,_),s(Pol,Bal,End),C),
		 lam(K,seq(dys(lam(Y,E),DRs,Conds),
			   lam(X,app(K,X)))),
		 to(to(to(Alpha,Beta),m(Gamma)),m(Gamma)))) :-
	nonvar(DyS), DyS=dys(E,DRs,Conds), !,
	punct_info(C,Bal,End).

% otherwise leave values on the bottom
% (no need to sequence, given LeftID equivalence)
% lam K . K Sem
% (beta -> M alpha) -> M alpha
monadic_lift(cat(C,Sem,Beta),
	     cat(tower(s(Pol,bal,_),s(Pol,Bal,End),C),
		 lam(K,app(K,Sem)),
		 to(to(Beta,m(Alpha)),m(Alpha)))) :-
	punct_info(C,Bal,End).




/*========================================================================
   Propagate punct info
========================================================================*/

punct_info(s(_,Bal,End),Bal2,End2) :- !, Bal=Bal2, End=End2.
punct_info(fslash(s(_,Bal,End),_),Bal2,End2) :- !, Bal=Bal2, End=End2.
punct_info(bslash(s(_,Bal,End),_),Bal2,End2) :- !, Bal=Bal2, End=End2.
punct_info(tower(_,s(_,Bal,End),_),Bal2,End2) :- !, Bal=Bal2, End=End2.
punct_info(_,_,_).

prop_punct_info(s(Pol,_,_),Bal,End,s(Pol,Bal,End)) :- !.
prop_punct_info(fslash(s(Pol,_,_),A),Bal,End,fslash(s(Pol,Bal,End),A)) :- !.
prop_punct_info(bslash(s(Pol,_,_),A),Bal,End,bslash(s(Pol,Bal,End),A)) :- !.
prop_punct_info(tower(L,s(Pol,_,_),A),Bal,End,tower(L,s(Pol,Bal,End),A)).


/*========================================================================
   Check for a negative category
========================================================================*/

neg_cat(cat(Syn,_,_)) :- neg_cat_syn(Syn).

neg_cat_syn(s(Pol,_,_)) :- Pol == -, !.
neg_cat_syn(tower(s(Pol,_,_),_,_)) :- Pol == -, !.
neg_cat_syn(tower(_,_,C)) :- neg_cat_syn(C).







