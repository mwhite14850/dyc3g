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
	( Num=0 -> print('*') ; true ),
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
% in accord with Steedman and contra Cooper/Keller storage and descendants
testcase("most customers in every charming restaurant order a juicy burger", 4).

% exceptional scope of indefinite in conditional, 2 readings
testcase("if a charming waiter snorts , Mia walks", 2).

% universals respect relative clause scope island, just 1 reading;
% one spurious reading from optional lowering that makes no difference,
% unlike with indefinites
testcase("if every charming waiter snorts , Mia walks", 1).

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

% basic coordination at tower level
testcase("a woman arrived and a man left", 1).

% coordination at non-tower (pure) level, of varying constituents
% nb: reading with distinct individuals for the indefinite implausible
%     but arguably conceivable
testcase("Vincent complains and Mia walks", 1).
testcase("Yolanda likes Burger King and loves Vincent", 1).
testcase("Butch orders and eats a juicy burger", 2).
testcase("Vincent orders and Jules eats a juicy burger", 2).

% mixed tower/non-tower coordination
testcase("a man complains and Mia walks", 1).
testcase("Yolanda likes Burger King and loves a juicy burger", 1).

% np-coord: distributive only
testcase("Vincent and Butch like Burger King",1).
testcase("Every man and every woman walks",1).
testcase("Jules likes every waiter and every customer",1).

% Geach examples: constraints on lifting yield only 2 readings
testcase("every man likes and every woman loves a juicy burger",2).
testcase("a woman loves every waiter and every customer",2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% negative polarity items
%

% 'anyone' not licensed under 'everyone'
testcase("everyone likes someone", 2).
testcase("everyone likes anyone", 0).

% 'anyone' licensed under 'no one'
testcase("no one likes anyone", 1).

% 'someone' must outscope 'no one', so just 1 reading
testcase("no one likes someone", 1).

% 'anyone' can't precede licensor
testcase("anyone likes no one", 0).

% NPIs must follow licensor with ditransitives
testcase("Kim gave no one anything", 1).
testcase("Kim gave anyone nothing", 0).

% reversed with PP[to]
testcase("Kim gave anything to no one", 0).
testcase("Kim gave nothing to anyone", 1).

% reversed again with <Bx
testcase("Kim gave to no one anything", 1).
testcase("Kim gave to anyone nothing", 0).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% punctuation examples
%

% unbalanced commas before end of sentence
testcase("Kim loves Sandy , CEO of XYZ .", 1).
testcase("Kim loves Sandy , CEO of XYZ , .", 0).

% balanced commas before another constituent
testcase("Kim loves Sandy , CEO of XYZ , madly .", 1).
testcase("Kim loves Sandy , CEO of XYZ madly .", 0).

% still works correctly with heavy-NP shift
testcase("Kim loves madly Sandy , CEO of XYZ Corp. .", 1).
testcase("Kim loves madly Sandy , CEO of XYZ Corp. , .", 0).

% balanced commas between arguments;
% final constituent determines clausal end punctuation
% nb: normal form doesn't deal with different conjunction orders,
%     otherwise there should be only 1 reading here
testcase("Kim gave Sandy , CEO of XYZ , a burger .", 2).
testcase("Kim gave Sandy , CEO of XYZ a burger .", 0).

% relative clause changes final constituent
% nb: likewise there should be 2 readings here rather than 3
testcase("Chris ordered a burger that Kim gave Sandy , CEO of XYZ .", 3).
testcase("Chris ordered a burger that Kim gave Sandy , CEO of XYZ , .", 0).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% examples from Baldridge's dissertation
%

testcase("Brazil defeated Germany", 1).
testcase("defeated Brazil Germany", 0).
testcase("Brazil played and defeated Germany", 1).
testcase("Brazil played Germany and defeated", 0).
testcase("Brazil defeated Germany and won the cup", 1).
testcase("John bought and gave her a book",2).
testcase("Brazil defeated Turkey and Germany",1).
testcase("team that defeated Germany", 1).
testcase("team that Brazil defeated", 1).
testcase("I thought Brazil defeated Germany", 1).
testcase("team that I thought Brazil defeated", 1).
testcase("team that I thought you said Brazil defeated", 1).
testcase("team that I thought you said that Brazil defeated", 1).
testcase("team that I thought you said John knew Brazil defeated", 1).
testcase("team that I thought you said John knew that Brazil defeated", 1).
testcase("Marcos picked up the ball", 1).
testcase("Marcos picked the ball up", 1).
% uniqueness of definites not implemented, so two readings here
testcase("Marcos threw and Ronaldo kicked the ball", 2).
testcase("Kahn blocked a shot by Rivaldo skillfully", 1).
% spurious ambiguity with conjunction not yet handled, so two readings here
testcase("Kahn blocked skillfully a powerful shot by Rivaldo", 2).
testcase("shot that Kahn blocked skillfully", 1).
testcase("China Brazil defeated", 1).
testcase("team that it beat previously", 1).
testcase("Brazil defeated yesterday the team that it beat previously", 2).
testcase("Rivaldo skillfully kicked the ball", 1).
testcase("ball that Rivaldo kicked", 1).
testcase("a player from Spain angrily left", 1).
testcase("the fan in the field left and in the stadium stayed", 0).
testcase("the fan in the field left and the fan in the stadium stayed", 4). % spurious def
testcase("the referee gave him today a well-deserved red card", 2). % spurious indef
% need <T for real reading here, but use with <Bx yields erroneous
% readings without use of inert slashes
testcase("the player that Fred gave a yellow card complains", 2).
% related example 'player that I read a book about' requires 'about' to
% have set args or to raise the noun over the noun modifier
testcase("I read a book about Rivaldo yesterday", 1).

