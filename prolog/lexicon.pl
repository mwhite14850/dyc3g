/*************************************************************************

    File: lexicon.pl
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

:- module(lexicon,[lexsign/3]).

:- use_module(grammar,[lexclass/3]).


/*========================================================================
   Lexical Lookup
========================================================================*/

% lookup lex sign
lexsign(Words,sign(LexWords,Cat,deriv([],[lex])),Rest) :-
	lexcat(Words,Cat,Rest),
	append(LexWords,Rest,Words).


/*========================================================================
   Lexical Classes
========================================================================*/

% instantiate lex cat from lex class for lex item
lexcat(Words,Cat,Rest) :-
	lexitem(Words-Rest,LexClass,Pred),
	lexclass(LexClass,Pred,Cat).

% lex cats can also be defined directly for function words;
% the discontiguous declaration allows them to appear later
:- discontiguous lexcat/3.


/*========================================================================
   Proper Names
========================================================================*/

lexitem([vincent|T]-T,pn,vincent).
lexitem([mia|T]-T,pn,mia).
lexitem([fred|T]-T,pn,fred).
lexitem([burger,king|T]-T,pn,bk).


/*========================================================================
   Intransitive Verbs
========================================================================*/

% nb: agreement not handled
lexitem([complains|T]-T,iv,complain).
lexitem([complain|T]-T,iv,complain).
lexitem([snorts|T]-T,iv,snort).
lexitem([snort|T]-T,iv,snort).
lexitem([walks|T]-T,iv,walk).
lexitem([walk|T]-T,iv,walk).
lexitem([wins|T]-T,iv,win).
lexitem([win|T]-T,iv,win).


/*========================================================================
   Transitive Verbs
========================================================================*/

lexitem([eats|T]-T,tv,eat).
lexitem([eat|T]-T,tv,eat).
lexitem([loves|T]-T,tv,love).
lexitem([love|T]-T,tv,love).
lexitem([likes|T]-T,tv,like).
lexitem([like|T]-T,tv,like).
lexitem([orders|T]-T,tv,order).
lexitem([order|T]-T,tv,order).


/*========================================================================
   Ditransitive Verbs
========================================================================*/

lexitem([gives|T]-T,dtv,give).
lexitem([give|T]-T,dtv,give).
lexitem([shows|T]-T,dtv,show).
lexitem([show|T]-T,dtv,show).


/*========================================================================
   Ditransitive + PP[for] Verbs
========================================================================*/

lexitem([trades|T]-T,dtv_for,trade).
lexitem([trade|T]-T,dtv_for,trade).


/*========================================================================
   Nouns
========================================================================*/

% nb: plurals not treated seriously, just there to go with 'most'
lexitem([woman|T]-T,n,woman).
lexitem([women|T]-T,n,woman).
lexitem([man|T]-T,n,man).
lexitem([men|T]-T,n,man).

lexitem([waiter|T]-T,n,waiter).
lexitem([waiters|T]-T,n,waiter).
lexitem([customer|T]-T,n,customer).
lexitem([customers|T]-T,n,customer).

lexitem([foot,massage|T]-T,n,footmassage).
lexitem([restaurant|T]-T,n,restaurant).
lexitem([burger|T]-T,n,burger).
lexitem([five,dollar,shake|T]-T,n,fdshake).

lexitem([prize|T]-T,n,prize).
lexitem([prizes|T]-T,n,prize).


/*========================================================================
   Relational Nouns
========================================================================*/

% relational nouns: 'friend of Y'
lexitem([friend|T]-T,rn,friend).
lexitem([neighbor|T]-T,rn,neighbor).
lexitem([aunt|T]-T,rn,aunt).


/*========================================================================
   Adjectives (attributive)
========================================================================*/

lexitem([charming|T]-T,adj,charming).
lexitem([juicy|T]-T,adj,juicy).


/*========================================================================
   Prepositions
========================================================================*/

% regular prep
lexitem([in|T]-T,prep,in).
lexitem([on|T]-T,prep,on).


/*========================================================================
   Case-Marking Prepositions
========================================================================*/

% adds prep to target cat, e.g. pp_of
lexitem([of|T]-T,case_prep,of).
lexitem([for|T]-T,case_prep,for).



/*========================================================================
   Conditional: 'if <A>, <B>'
========================================================================*/

% conditional
lexcat([if|Words],
       cat(fslash(fslash(s,delim(s)),delim(s)),
	   lam(A,lam(B,dys(imp(A,B),[],[]))),
	   to(m(t),to(m(t),m(t)))),
       Words).


/*========================================================================
   Sentential Negation
========================================================================*/

% makes its complement a scope island
lexcat([it,is,not,the,case,that|Words],Cat,Words) :-
	lexcat([no,way|Words],Cat,Words).

lexcat([no,way|Words],
       cat(fslash(s,delim(s)),
	   lam(A,dys(not(A),[],[])),
	   to(m(t),m(t))),
       Words).


/*========================================================================
   Relative Pronouns
========================================================================*/

% subj rel
% (e -> t) -> (e -> t) -> e -> t
lexcat([who|Words],
       cat(fslash(bslash(n,n),delim(bslash(s,np))),
	   lam(Q,lam(P,lam(X,and(app(P,X),app(Q,X))))),
	   to(to(e,t),to(to(e,t),to(e,t)))),
       Words).

% obj rel
% (e -> t) -> (e -> t) -> e -> t
lexcat([who|Words],
       cat(fslash(bslash(n,n),delim(fslash(s,np))),
	   lam(Q,lam(P,lam(X,and(app(P,X),app(Q,X))))),
	   to(to(e,t),to(to(e,t),to(e,t)))),
       Words).



/*========================================================================
   Quantificational Pronouns
========================================================================*/

lexcat([everyone|Words],
       cat(tower(s,s,np),
	   lam(K,dys(all(X,dys(person(X),[],[]),app(K,X)),[],[])),
	   to(to(e,m(t)),m(t))),
       Words).

lexcat([someone|Words],
       cat(tower(s,s,np),
	   lam(K,seq(dys(X,[X],[person(X)]),lam(Y,app(K,Y)))),
	   to(to(e,m(Alpha)),m(Alpha))),
       Words).

lexcat([no,one|Words],
       cat(tower(s,s,np),
	   lam(K,dys(not(seq(dys(X,[X],[person(X)]),
			     lam(Y,app(K,Y)))),
		     [],[])),
	   to(to(e,m(t)),m(t))),
       Words).

lexcat([something|Words],
       cat(tower(s,s,np),
	   lam(K,seq(dys(X,[X],[thing(X)]),lam(Y,app(K,Y)))),
	   to(to(e,m(Alpha)),m(Alpha))),
       Words).


/*========================================================================
   Determiners
========================================================================*/

% this implements Charlow's lowering approach to dets
% (((e -> t) -> t) -> Mt) -> (e -> M alpha) -> M alpha
lexcat([every|Words],
       cat(tower(tower(s,s,np),s,
		 fslash(s,n)),
	   lam(K,lam(K2,dys(all(X,
				app(K,lam(P,app(P,X))),
				app(K2,X)),[],[]))),
	   to(to(to(to(e,t),t),m(t)),to(to(e,m(t)),m(t)))),
       Words).

lexcat([most|Words],
       cat(tower(tower(s,s,np),s,
		 fslash(s,n)),
	   lam(K,lam(K2,dys(most(X,
				 app(K,lam(P,app(P,X))),
				 app(K2,X)),[],[]))),
	   to(to(to(to(e,t),t),m(t)),to(to(e,m(t)),m(t)))),
       Words).

lexcat([few|Words],
       cat(tower(tower(s,s,np),s,
		 fslash(s,n)),
	   lam(K,lam(K2,dys(few(X,
				app(K,lam(P,app(P,X))),
				app(K2,X)),[],[]))),
	   to(to(to(to(e,t),t),m(t)),to(to(e,m(t)),m(t)))),
       Words).

% (((e -> t) -> t) -> Mt) -> (e -> M alpha) -> M alpha
lexcat([a|Words],
       cat(tower(tower(s,s,np),s,
		 fslash(s,n)),
	   lam(K,lam(K2,seq(seq_st(dys(X,[X],[]),lam(Z,app(K,lam(P,app(P,Z))))),
			    lam(Y,app(K2,Y))))),
	   to(to(to(to(e,t),t),m(t)),to(to(e,m(Alpha)),m(Alpha)))),
       Words).


/*========================================================================
   Conjunction
   TODO
========================================================================*/

% from Barker and Shan, plus dynamic
%lexcat([and|Words],
%       cat(fslash(bslash(tower(s,s,X),
%			 tower(s,s,X)),
%		  tower(s,s,X)),
%	   lam(R,lam(L,lam(K,seq(app(L,K),
%				 lam(P,seq(app(R,K),
%					   lam(Q,dys(and(P,Q),[],[]))))))))),
%       Words).

