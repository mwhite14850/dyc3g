/*************************************************************************

    File: regression.pl
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

:- module(regression,[regression/0]).

:- use_module(dyc3g,[continuized/2]).
:- use_module(readLine,[]).
:- use_module(filterAlph,[filterAlphabeticVariants/2,number_list/3]).


/*========================================================================
   Driver Predicate
========================================================================*/

regression :-
	findall(item(Sent,Num),testcase(Sent,Num),Items), nl,
	print('Status/#Readings/#Expected/#Derivations/Sentence:'), nl, nl,
	time(testitems(Items)), nl.

testitems(Items) :- testitems(Items,0,0,0,0,0).

testitems([],Good,Bad,TotalR,TotalD,TotalLen) :- nl,
	print('found '), print(TotalD), print(' derivations for '),
	print(TotalR), print(' readings'), nl,
	Total is Good + Bad,
	print('across '), print(Total), print(' test items of total length '),
	print(TotalLen), nl,
	print('with '), print(Good), print(' success(es) and '),
	print(Bad), print(' failure(s)'), nl.

testitems([item(Sent,Num)|Rest],Good0,Bad0,TotalR0,TotalD0,TotalLen0) :-
	split_string(Sent,"\s\t\n","\s\t\n",L),
	readLine:checkWords(L,Words),
	length(Words,Len), TotalLen is TotalLen0 + Len,
        findall(Sign,continuized(Words,Sign),Signs),
	length(Signs,NumDerivs),
	TotalD is TotalD0 + NumDerivs,
	findall(Sem,member(sign(_,cat(_,Sem,_),_),Signs),Sems),
	number_list(Sems,Sems1,1),
	filterAlphabeticVariants(Sems1,Sems2),
	length(Sems2,NumReadings),
	TotalR is TotalR0 + NumReadings,
	( Num==NumReadings
	-> ( (Num==NumDerivs -> Status = 'ok' ; Status = 'ok+'),
	     Good is Good0 + 1, Bad = Bad0)
	; (Status = 'fail', Good = Good0, Bad is Bad0 + 1) ),
	print(Status), print('\t'),
	print(NumReadings), print('\t'),
	print(Num), print('\t'),
	print(NumDerivs), print('\t'),
	print(Sent), nl,
	testitems(Rest,Good,Bad,TotalR,TotalD,TotalLen).


/*========================================================================
   Test case: sentence, expected number of readings
========================================================================*/

% basic 2-way ambiguity in a transitive sentence
testcase("everyone likes someone", 2).

% 2-way ambiguity with an indefinite determiner
testcase("everyone loves a charming restaurant", 2).

% 6-way ambiguity with ditransitive
testcase("most waiters give everyone a juicy burger", 6).

% 6-way ambiguity with ditransitive+PP and simple subject
testcase("Vincent trades every customer a burger for few prizes", 6).

% 24-way ambiguity with ditransitive+PP
testcase("most waiters trade everyone a burger for few prizes", 24).

% 4-way ambiguity with PP-inversion crossed with object inversion,
% in accord with Steedman and contra Cooper/Keller storage and
% descendants
testcase("most customers in every charming restaurant order a juicy burger", 4).

% exceptional scope of indefinite in conditional, 2 readings
testcase("if a charming waiter snorts, Mia walks", 2).

% universals respect relative clause scope island, just 1 reading;
% one spurious reading from optional lowering that makes no difference,
% unlike with indefinites
testcase("if every charming waiter snorts, Mia walks", 1).

% exceptional scope of indefinite in relative clause, 2 readings
testcase("every customer who likes a charming restaurant walks", 2).

% universals respect relative clause scope island, just 1 reading;
% one spurious reading from optional lowering that makes no difference,
% unlike with indefinites
testcase("a customer who likes every charming restaurant walks", 1).

% same for object relatives
testcase("every customer who a charming waiter likes complains", 2).
testcase("a customer who every charming waiter likes complains", 1).

% inversion from PP, unlike relative clause
testcase("a customer in every charming restaurant complains", 2).

% scoping from middle of NP, not possible with Steedman's inverse
% linking account; 4 readings, including non-sensical ones where
% restaurants complain and customers are in every restaurant
testcase("most customers in every restaurant who complain like Vincent", 4).

% only two derivations, one for each reading: non-NF spuriously
% equivalent reading with three-level tower eliminated even though it's
% not directly under the lower operation
testcase("Vincent gives everyone a five dollar shake", 2).

% spurious ambiguity with indefinites not yet handled though, so two
% readings here
testcase("a customer orders a juicy burger", 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% examples from Baldridge's dissertation
%

testcase("Brazil defeated Germany", 1).
testcase("defeated Brazil Germany", 0).
testcase("team that defeated Germany", 1).
testcase("team that Brazil defeated", 1).
testcase("I thought Brazil defeated Germany", 1).
testcase("team that I thought Brazil defeated", 1).
testcase("team that I thought you said Brazil defeated", 1).
testcase("team that I thought you said that Brazil defeated", 1).
testcase("team that I thought you said John knew Brazil defeated", 1).
testcase("team that I thought you said John knew that Brazil defeated", 1).
testcase("Marcos picked up a ball", 1).
testcase("Marcos picked a ball up", 1).
testcase("Kahn blocked a shot by Rivaldo skillfully", 1).
% spurious ambiguity with conjunction not yet handled, so two readings here
testcase("Kahn blocked skillfully a powerful shot by Rivaldo", 2).
testcase("shot that Kahn blocked skillfully", 1).
testcase("China Brazil defeated", 1).
testcase("team that it beat previously", 1).
testcase("Brazil defeated yesterday a team that it beat previously", 2).
testcase("Rivaldo skillfully kicked a ball", 1).
testcase("ball that Rivaldo kicked", 1).
testcase("a player from Spain angrily left", 1).
testcase("a referee gave him today a well-deserved red card", 2). % spurious indef
% need <T for real reading here, but use with <Bx yields erroneous
% readings without use of inert slashes
testcase("a player that Fred gave a yellow card complains", 2).
% related example 'player that I read a book about' requires 'about' to
% have set args or to raise the noun over the noun modifier
testcase("I read a book about Rivaldo yesterday", 1).
