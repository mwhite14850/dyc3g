/*************************************************************************

    File: grammar.pl
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

:- module(grammar,[typeraise/3,lexclass/3]).


/*========================================================================
   Type Raising (grammar specific)
   nb: also type-changing, ie unary rules with possibly different semantics
========================================================================*/

%                 np : X : e
% ----------------------------------------
% s/(s\np) : lam P . (P X) : (e -> t) -> t
typeraise(cat(Cat1,Sem1,Type1),cat(Cat,Sem,Type),Op) :-
	Cat1=np, Cat=fslash(s(_,_,_),bslash(s(_,_,_),np)),
	Sem=lam(P,app(P,Sem1)),
	Type1=e, Type=to(to(e,t),t),
	Op = ['>T'].

% np => s\(s/np)
%typeraise(cat(Cat1,Sem1,Type1),cat(Cat,Sem,Type),Op) :-
%	Cat1=np, Cat=bslash(s,fslash(s,np)),
%	Sem=lam(P,app(P,Sem1)),
%	Type1=e, Type=to(to(e,t),t),
%	Op = ['<T'].

% nb: this works but need inert slash modality to avoid
%     incorrect reading of 'Fred gave him a yellow card'
%     with roles reversed using <Bx.
% np => (s\np)\((s\np)/np)
%typeraise(cat(Cat1,Sem1,Type1),cat(Cat,Sem,Type),Op) :-
%	Cat1=np, Cat=bslash(bslash(s,np),fslash(bslash(s,np),np)),
%	Sem=lam(P,app(P,Sem1)),
%	Type1=e, Type=to(to(e,to(e,t)),to(e,t)),
%	Op = ['<T'].

% for heavy-NP shift
% pp_to => (s\np)\((s\np)/pp_to)
typeraise(cat(Cat1,Sem1,Type1),cat(Cat,Sem,Type),Op) :-
	Cat1=pp_to, Cat=bslash(bslash(s(_,_,_),np),fslash(bslash(s(_,_,_),np),pp_to)),
	Sem=lam(P,app(P,Sem1)),
	Type1=e, Type=to(to(e,to(e,t)),to(e,t)),
	Op = ['<T'].

% topicalization, ignoring info struct
% np => s/(s/np)
typeraise(cat(Cat1,Sem1,Type1),cat(Cat,Sem,Type),Op) :-
	Cat1=np, Cat=fslash(s(_,_,_),fslash(s(_,_,_),np)),
	Sem=lam(P,app(P,Sem1)),
	Type1=e, Type=to(to(e,t),t),
	Op = ['Top'].

% predicative NP
% n => np_pred
typeraise(cat(n,Sem,Type),cat(np_pred,Sem,Type),['N_to_NP_pred']).


/*========================================================================
   Proper Names
========================================================================*/

lexclass(pn,Pred,cat(np,Pred,e)).


/*========================================================================
   Intransitive Verbs
========================================================================*/

% nb: agreement not handled
lexclass(iv,Pred,cat(bslash(s(_,_,_),np),lam(X,Prop),to(e,t))) :-
	Prop =.. [Pred,X].


/*========================================================================
   Transitive Verbs
========================================================================*/

lexclass(tv,Pred,cat(fslash(bslash(s(_,_,_),np),np),lam(Y,lam(X,Prop)),to(e,to(e,t)))) :-
	 Prop =.. [Pred,X,Y].


/*========================================================================
   Particle Verbs
========================================================================*/

lexclass(tv_prt,[Pred,Prt],
	 cat(fslash(fslash(bslash(s(_,_,_),np),np),PrtCat),
	     lam(_,lam(Y,lam(X,Prop))),
	     to(e,to(e,to(e,t))))) :-
	 Prop =.. [Pred,X,Y], concat('prt_',Prt,PrtCat).

lexclass(tv_prt,[Pred,Prt],
	 cat(fslash(fslash(bslash(s(_,_,_),np),PrtCat),np),
	     lam(Y,lam(_,lam(X,Prop))),
	     to(e,to(e,to(e,t))))) :-
	 Prop =.. [Pred,X,Y], concat('prt_',Prt,PrtCat).


/*========================================================================
   Ditransitive Verbs
========================================================================*/

lexclass(dtv,Pred,
	 cat(fslash(fslash(bslash(s(_,_,_),np),np),np),
	     lam(Y,lam(Z,lam(X,Prop))),
	     to(e,to(e,to(e,t))))) :-
	Prop =.. [Pred,X,Y,Z].


/*========================================================================
   Ditransitive Verbs with PP[to]
========================================================================*/

lexclass(tv_to,Pred,
	 cat(fslash(fslash(bslash(s(_,_,_),np),pp_to),np),
	     lam(Y,lam(Z,lam(X,Prop))),
	     to(e,to(e,to(e,t))))) :-
	Prop =.. [Pred,X,Z,Y].


/*========================================================================
   Ditransitive + PP[for] Verbs
========================================================================*/

lexclass(dtv_for,Pred,
	 cat(fslash(fslash(fslash(bslash(s(_,_,_),np),pp_for),np),np),
	     lam(Y,lam(Z,lam(W,lam(X,Prop)))),
	     to(e,to(e,to(e,to(e,t)))))) :-
	Prop =.. [Pred,X,Y,Z,W].


/*========================================================================
   Sentential Complement Verbs
========================================================================*/

lexclass(scompv,Pred,cat(fslash(bslash(s(_,_,_),np),s(_,_,_)),
			 lam(S,lam(X,Prop)),to(t,to(e,t)))) :-
	 Prop =.. [Pred,X,S].


/*========================================================================
   Nouns
========================================================================*/

% nb: plurals not treated seriously, just there to go with 'most'
lexclass(n,Pred,cat(n,lam(X,Prop),to(e,t))) :-
	Prop =.. [Pred,X].


/*========================================================================
   Relational Nouns
========================================================================*/

% e.g. 'friend of Y'
lexclass(rn,Pred,
	 cat(fslash(n,pp_of),
	     lam(Y,lam(X,Prop)),
	   to(e,to(e,t)))) :-
	Prop =.. [Pred,X,Y].


/*========================================================================
   Adjectives (attributive)
========================================================================*/

lexclass(adj,Pred,
	 cat(fslash(n,n),lam(P,lam(X,and(app(P,X),Prop))),
	     to(to(e,t),to(e,t)))) :-
	Prop =.. [Pred,X].


/*========================================================================
   Prepositions
========================================================================*/

% regular prep
lexclass(prep,Pred,
	 cat(fslash(bslash(n,n),np),
	     lam(Y,lam(P,lam(X,and(app(P,X),Prop)))),
	     to(e,to(to(e,t),to(e,t))))) :-
	Prop =.. [Pred,X,Y].


/*========================================================================
   Case-Marking Prepositions
========================================================================*/

% adds prep to target cat, e.g. pp_of
lexclass(case_prep,Prep,cat(fslash(Target,np),lam(X,X),to(e,e))) :-
	concat('pp_',Prep,Target).


/*========================================================================
   Particles
========================================================================*/

% adds particle to target cat, e.g. prt_up
lexclass(prt,Prt,cat(Target,unit,e)) :-
	concat('prt_',Prt,Target).


/*========================================================================
   Adverbs
========================================================================*/

% should really be an event modifier
lexclass(adv,Pred,
	 cat(bslash(s(_,_,_),s(_,_,_)),
	     lam(P,Prop),
%	     lam(P,and(P,Pred)),
	     to(t,t))) :-
	Prop =.. [Pred,P].

% nb: can use <Bx with subj to get pre-verbal position
%lexclass(adv,Pred,
%	 cat(fslash(bslash(s,np),bslash(s,np)),
%	     lam(P,lam(X,and(app(P,X),Pred))),
%	     to(to(e,t),to(e,t)))).


/*========================================================================
   Pronouns
========================================================================*/

% ignoring anaphora and agreement
lexclass(pro,Pred,cat(np,Pred,e)).


/*========================================================================
   Relative Pronouns
========================================================================*/

% subj rel
% (e -> t) -> (e -> t) -> e -> t
lexclass(relpro_subj,_,
	 cat(fslash(bslash(n,n),delim(bslash(s(_,_,_),np))),
	     lam(Q,lam(P,lam(X,and(app(P,X),app(Q,X))))),
	     to(to(e,t),to(to(e,t),to(e,t))))).

% obj rel
% (e -> t) -> (e -> t) -> e -> t
lexclass(relpro_obj,_,
	 cat(fslash(bslash(n,n),delim(fslash(s(_,_,_),np))),
	     lam(Q,lam(P,lam(X,and(app(P,X),app(Q,X))))),
	     to(to(e,t),to(to(e,t),to(e,t))))).


