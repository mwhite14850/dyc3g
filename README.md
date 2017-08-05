# dyc3g
Dynamic Continuized Combinatory Categorial Grammar Library


# Introduction

This project implements a prototype parser for Dynamic Continuized
Combinatory Categorial Grammar, as described in "Parsing with Dynamic
Continuized CCG" by Michael White, Simon Charlow, Jordan Needle and
Dylan Bumford, to appear in the Proceedings of the 13th International
Workshop on Tree-Adjoining Grammar and Related Formalisms
([TAG+13](http://tag13.cs.umu.se/)), 2017.

It (1) demonstrates Barker and Shan's (2015) approach to
handling quantifiers as functions on their own continuations in the
'tower' system, using lift operations integrated with binary
combination steps; (2) uses CCG combinators on the tower bottom; (3)
enforces scope islands by having categories that combine with finite
clauses require them to be delimited, together with delimited
combination rules that lower the delimited argument; and (4)
implements Charlow's (2014) treatment of the exceptional scope of
indefinites using a dynamic semantics with the state-set monad. 


# License

This file uses code from BB1, version 1.3 (November 2006), and shares
the same license.


# Requirements

* SWI-Prolog version 7.4 or later


# Usage

See prolog/LOG for example usage.
