/*************************************************************************

    File: dyc3g.pl

    This program (1) demonstrates Barker and Shan's (2015) approach to
    handling quantifiers as functions on their own continuations in the
    'tower' system, using lift operations integrated with binary
    combination steps; (2) uses CCG combinators on the tower bottom; (3)
    enforces scope islands by having categories that combine with finite
    clauses require them to be delimited, together with delimited
    combination rules that lower the delimited argument; and (4)
    implements Charlow's (2014) to the exceptional scope of indefinites
    using a dynamic semantics with the state-set monad.

    Copyright (C) 2017 Michael White

    This file uses code from BB1, version 1.3 (November 2006), and
    shares the same license.

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

:- module(dyc3g,[continuized/0,continuized/2,regression/0,
		 infix/0,prefix/0,derivs/0,noderivs/0]).

:- use_module(readLine,[readLine/1]).
:- use_module(comsemPredicates,[infix/0,prefix/0,printRepresentations/1]).

:- use_module(rules,[combine/3,combine/4]).
:- use_module(monadic,[lower_result_tower/4,lower_if_poss/2,neg_cat/1]).
:- use_module(normalForm,[normal_form/1]).
:- use_module(reduce,[reduce/2,reduce_sign/2]).
:- use_module(filterAlph,[filterAlphabeticVariants/2,number_list/3]).
:- use_module(showDerivs,[derivs/0,noderivs/0,showDerivList/1]).
:- use_module(lexicon,[lexsign/3]).
:- use_module(regression,[regression/0]).


/*========================================================================
   Driver Predicates
========================================================================*/

continuized :-
	readLine(Sentence),
        findall(Sign,continuized(Sentence,Sign),Signs),
	showDerivList(Signs),
	findall(Sem,member(sign(_,cat(_,Sem,_),_),Signs),Sems),
	number_list(Sems,Sems1,1),
	filterAlphabeticVariants(Sems1,Sems2),
	printRepresentations(Sems2), !.

continuized(Sentence,Sign):-
	sr_parse([],Sentence,Sign0),       % shift-reduce parse
	Sign0 = sign(W,Cat,D),             % extract Cat and Sem
	Cat = cat(Syn,Sem,Type),
	reduce(Sem,SemR),                  % beta and sequence reduce
	Sign = sign(W,cat(Syn,SemR,Type),D).


/*========================================================================
   Shift-Reduce Parser: Stack, Buffer, Result
========================================================================*/

% done when buffer empty and a single item remains
% do final lowering if possible,
% ensure not a negative cat
sr_parse([Sign0],[],Sign) :-
	lower_if_poss(Sign0,Sign),
	Sign = sign(_,Cat,_),
	\+ neg_cat(Cat),
	normal_form(Sign).

% reduce binary: replace top two signs on stack with combination
sr_parse([Sign2,Sign1|Stack],Words,Result) :-
	combine_binary(Sign1,Sign2,Sign),
	normal_form(Sign),
	reduce_sign(Sign,SignR),
	sr_parse([SignR|Stack],Words,Result).

% reduce unary: replace top sign on stack with combination
sr_parse([Sign1|Stack],Words,Result) :-
	combine_unary(Sign1,Sign),
	normal_form(Sign),
	reduce_sign(Sign,SignR),
	sr_parse([SignR|Stack],Words,Result).

% shift: lookup lex sign, push onto stack
sr_parse(Stack,Words,Result) :-
	lexsign(Words,Sign,Rest),
	sr_parse([Sign|Stack],Rest,Result).


/*========================================================================
   Category Combination:
     signs are sign(Words,Cat,Deriv)
     categories are cat(Syn,Sem,Type)
     syn cats are fslash(Res,Arg), bslash(Res,Arg) or tower(Res,Over,Loc)
       for forward/backward slash and tower;
       island cats of the form Res/<Arg> or Res\<Arg> are represented
       by fslash(Res,delim(Arg)) or bslash(Res,delim(Arg))
========================================================================*/

% combine two signs, lowering a result tower if apropos
combine_binary(sign(W1,Cat1,D1),sign(W2,Cat2,D2),sign(W,Cat,D)) :-
	combine(Cat1,Cat2,Cat0,Op0),
	append(W1,W2,W),
	lower_result_tower(Cat0,Cat,Op0,Op),
	D = deriv([sign(W1,Cat1,D1),sign(W2,Cat2,D2)],Op).

% combine single sign with unary rule
combine_unary(sign(W,Cat1,D1),sign(W,Cat,D)) :-
	combine(Cat1,Cat,Op),
	D = deriv([sign(W,Cat1,D1)],Op).



/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n> dyc3g.pl, by Michael White                                          <',[]),
   format('~n>                                                                     <',[]),
   format('~n> ?- regression.               - run regression test suite            <',[]),
   format('~n> ?- continuized.              - parse a sentence using continuations <',[]),
   format('~n> ?- derivs.                   - switches derivations on              <',[]),
   format('~n> ?- noderivs.                 - switches derivations off             <',[]),
   format('~n> ?- info.                     - shows this information               <',[]),
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.


