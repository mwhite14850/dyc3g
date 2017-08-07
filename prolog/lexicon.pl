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
lexitem([butch|T]-T,pn,butch).
lexitem([jules|T]-T,pn,jules).
lexitem([mia|T]-T,pn,mia).
lexitem([yolanda|T]-T,pn,yolanda).
lexitem([burger,king|T]-T,pn,bk).

lexitem([brazil|T]-T,pn,brazil).
lexitem([china|T]-T,pn,china).
lexitem([germany|T]-T,pn,germany).
lexitem([spain|T]-T,pn,spain).
lexitem([turkey|T]-T,pn,turkey).
lexitem([fred|T]-T,pn,fred).
lexitem([john|T]-T,pn,john).
lexitem([kahn|T]-T,pn,kahn).
lexitem([marcos|T]-T,pn,marcos).
lexitem([rivaldo|T]-T,pn,rivaldo).
lexitem([ronaldo|T]-T,pn,ronaldo).


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

lexitem([arrived|T]-T,iv,arrive).
lexitem([came|T]-T,iv,come).
lexitem([left|T]-T,iv,leave).
lexitem([stayed|T]-T,iv,stay).
lexitem([won|T]-T,iv,win).


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
lexitem([wins|T]-T,tv,win).
lexitem([win|T]-T,tv,win).

lexitem([beat|T]-T,tv,beat).
lexitem([blocked|T]-T,tv,block).
lexitem([bought|T]-T,tv,buy).
lexitem([defeated|T]-T,tv,defeat).
lexitem([kicked|T]-T,tv,kick).
lexitem([played|T]-T,tv,play).
lexitem([read|T]-T,tv,read).
lexitem([threw|T]-T,tv,throw).
lexitem([won|T]-T,tv,win).


/*========================================================================
   Particle Verbs
========================================================================*/

lexitem([picked|T]-T,tv_prt,[pick_up,up]).


/*========================================================================
   Ditransitive Verbs
========================================================================*/

lexitem([gives|T]-T,dtv,give).
lexitem([give|T]-T,dtv,give).
lexitem([gave|T]-T,dtv,give).
lexitem([shows|T]-T,dtv,show).
lexitem([show|T]-T,dtv,show).


/*========================================================================
   Ditransitive + PP[for] Verbs
========================================================================*/

lexitem([trades|T]-T,dtv_for,trade).
lexitem([trade|T]-T,dtv_for,trade).


/*========================================================================
   Sentential Complement Verbs
========================================================================*/

lexitem([said|T]-T,scompv,say).
lexitem([thought|T]-T,scompv,think).
lexitem([knew|T]-T,scompv,know).


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

lexitem([ball|T]-T,n,ball).
lexitem([book|T]-T,n,book).
lexitem([card|T]-T,n,card).
lexitem([cup|T]-T,n,cup).
lexitem([fan|T]-T,n,fan).
lexitem([field|T]-T,n,field).
lexitem([player|T]-T,n,player).
lexitem([referee|T]-T,n,referee).
lexitem([shot|T]-T,n,shot).
lexitem([stadium|T]-T,n,stadium).
lexitem([team|T]-T,n,team).


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

lexitem([powerful|T]-T,adj,powerful).
lexitem([red|T]-T,adj,red).
lexitem([welldeserved|T]-T,adj,well_deserved).
lexitem([yellow|T]-T,adj,yellow).


/*========================================================================
   Prepositions
========================================================================*/

% regular prep
lexitem([about|T]-T,prep,about).
lexitem([by|T]-T,prep,by).
lexitem([from|T]-T,prep,from).
lexitem([in|T]-T,prep,in).
lexitem([on|T]-T,prep,on).


/*========================================================================
   Case-Marking Prepositions
========================================================================*/

% adds prep to target cat, e.g. pp_of
lexitem([of|T]-T,case_prep,of).
lexitem([for|T]-T,case_prep,for).


/*========================================================================
   Particles
========================================================================*/

lexitem([up|T]-T,prt,up).


/*========================================================================
   Adverbs
========================================================================*/

lexitem([angrily|T]-T,adv,angry).
lexitem([previously|T]-T,adv,previous).
lexitem([skillfully|T]-T,adv,skillful).
lexitem([today|T]-T,adv,today).
lexitem([yesterday|T]-T,adv,yesterday).


/*========================================================================
   Pronouns
========================================================================*/

% ignoring anaphora and agreement
lexitem([i|T]-T,pro,i).
lexitem([you|T]-T,pro,you).
lexitem([him|T]-T,pro,he).
lexitem([her|T]-T,pro,she).
lexitem([it|T]-T,pro,it).


/*========================================================================
   Relative Pronouns
========================================================================*/

% ignoring animacy for 'who(m)'
lexitem([who|T]-T,relpro_subj,_).
lexitem([who|T]-T,relpro_obj,_).
lexitem([whom|T]-T,relpro_obj,_).

lexitem([that|T]-T,relpro_subj,_).
lexitem([that|T]-T,relpro_obj,_).


/*========================================================================
   Complementizer
========================================================================*/

% this is simplified wrt CCGbank analysis
lexcat([that|Words],
       cat(fslash(s,s),
	   lam(X,X),
	   to(t,t)),
       Words).


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

% for now, just add definiteness annotation
lexcat([the|Words],
       cat(tower(tower(s,s,np),s,
		 fslash(s,n)),
	   lam(K,lam(K2,seq(seq_st(dys(X,[X],[def(X)]),lam(Z,app(K,lam(P,app(P,Z))))),
			    lam(Y,app(K2,Y))))),
	   to(to(to(to(e,t),t),m(t)),to(to(e,m(Alpha)),m(Alpha)))),
       Words).


/*========================================================================
   Conjunction
========================================================================*/

% from Barker and Shan, plus dynamic
%
% coordination of like tower types with X:Alpha on the bottom,
% so sem type is T -> T -> T where T is (alpha -> Mt) -> Mt
lexcat([and|Words],
       cat(fslash(bslash(tower(s,s,X),
			 tower(s,s,X)),
		  tower(s,s,X)),
	   lam(R,lam(L,lam(K,seq(app(L,K),
				 lam(P,seq(app(R,K),
					   lam(Q,dys(and(P,Q),[],[])))))))),
	   to(to(to(Alpha,m(t)),m(t)),
	      to(to(to(Alpha,m(t)),m(t)),
		 to(to(Alpha,m(t)),m(t))))),
       Words).


