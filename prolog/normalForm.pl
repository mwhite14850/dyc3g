/*************************************************************************

    File: normalForm.pl
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

:- module(normalForm,[normal_form/1]).


/*========================================================================
   Normal Form Derivations contain no patterns indicating a normal form
   violation
========================================================================*/

normal_form(Sign) :- \+ normal_form_violation(Sign).

% check for CCG normal form violations
normal_form_violation(Sign) :- ccg_normal_form_violation(Sign).

% avoid LiftR_LiftL right before a Lower, as this is equivalent to the
% straight sequential combo
normal_form_violation(sign(_,_,deriv([Sign],Op))) :-
	member('Lower',Op),
	has_LiftR_LiftL(Sign).

% avoid LiftR_LiftL right before a DelimitR or DelimitL
normal_form_violation(sign(_,_,deriv([_,Sign],Op))) :-
	member('DelimR',Op),
	has_LiftR_LiftL(Sign).

normal_form_violation(sign(_,_,deriv([Sign,_],Op))) :-
	member('DelimL',Op),
	has_LiftR_LiftL(Sign).

% TODO: avoid LiftL_LiftR inversion with just indefinites
%

% check for spurious LiftR_LiftL
has_LiftR_LiftL(sign(_,_,deriv(_,Op))) :-
	append([_,['LiftR','LiftL'],_],Op).

% recurse past inversion
has_LiftR_LiftL(sign(_,_,deriv([Sign1,Sign2],Op))) :-
	append([_,['LiftL','LiftR'],_],Op),
	( has_LiftR_LiftL(Sign1) ; has_LiftR_LiftL(Sign2) ).

% recurse past non-scopal signs
has_LiftR_LiftL(sign(_,_,deriv([Sign1,Sign2],_))) :-
	Sign1 = sign(_,cat(C,_,_),_), \+ C=tower(_,_,_),
	has_LiftR_LiftL(Sign2).

has_LiftR_LiftL(sign(_,_,deriv([Sign1,Sign2],_))) :-
	Sign2 = sign(_,cat(C,_,_),_), \+ C=tower(_,_,_),
	has_LiftR_LiftL(Sign1).

% recurse past Comb_LiftL on left: combination on top level spurious
% nb: LiftL_Comb nec. to get all > most > some in
% 'most waiters give every customer a burger'
% where LiftR_LiftL is at the top of Sign2
has_LiftR_LiftL(sign(_,_,deriv([_Sign1,Sign2],Op))) :-
	['Comb','LiftL'|_] = Op,
	has_LiftR_LiftL(Sign2).

% TODO: recurse past Lift_Lift_Comb on left, which would require
% tracking at what level LiftR_LiftL occurs on right in order to be safe
% (and similarly below)

% recurse past LiftR_Comb on right, similarly
% nb: not symmetric! Comb_LiftR nec. to get all > few > some in
% 'Vincent trades every customer a burger for few prizes'
% where LiftR_LiftL is at the top of Sign1
has_LiftR_LiftL(sign(_,_,deriv([Sign1,_Sign2],Op))) :-
	['LiftR','Comb'|_] = Op,
	has_LiftR_LiftL(Sign1).


/*========================================================================
   CCG Normal Form Constraints from Eisner (1996) and
   Hockenmaier and Bisk (2010) in the form of disallowed
   (not complete)
========================================================================*/

ccg_normal_form_violation(sign(_,_,D)) :-
	D = deriv([sign(_,_,D1),sign(_,_,D2)],Op),
	last(Op,BotOp),
	D1 = deriv(_,Op1), D2 = deriv(_,Op2),
	last(Op1,BotOp1), last(Op2,BotOp2),
	ccg_normal_form_violation(BotOp1,BotOp2,BotOp).

ccg_normal_form_violation('>T',_,'>').
ccg_normal_form_violation('>B',_,'>').
ccg_normal_form_violation('>B',_,'>B').

% avoid coordinating already raised cats
ccg_normal_form_violation('lex','>T','>').
ccg_normal_form_violation('lex','Top','>').
