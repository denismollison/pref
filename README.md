
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pref

<!-- badges: start -->

[![R-CMD-check](https://github.com/denismollison/pref/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/denismollison/pref/actions/workflows/R-CMD-check.yaml)
<!-- badges: end --> Preference voting with explanatory graphics

There is a full manual pref_pkg_manual.pdf

The main functions count votes from a Single Transferable Vote (STV)
election, and illustrate the count with clear explanatory graphics. The
core option is Meek STV, which is the purest expression of the simple
principles of STV (see Section 7 of the manual), but requires electronic
counting. It can handle votes expressing equal preferences for subsets
of the candidates. The function stv.wig implements the Weighted
Inclusive Gregory method, as used in Scottish council elections, with
the same options as stv, as described below. The functions stv and
stv.wig require vote data input as an R list, whose essential component
is a vote data matrix, but which can contain details such as an election
title, and candidates’ parties and party colours. The function pref.data
can translate various standard data formats into the required R list
format.

Six examples are provided, drawn from real elections, hc12, p17, nws17,
cnc17, yale and j02. The first four are from Scottish Council elections,
the first a very simple case and the second a more usual one. The third
example (nws17) is one where using the official counting method (WIG) no
candidate achieves the fixed quota. The fourth is one where Meek and WIG
STV produce different outcomes. The fifth, from a Yale faculty election,
is an example with a large number of candidates (44 for 4 places). A
simplified version, showing just the later stages of this election is
provided as y12.

The final example, j02, is from a charity’s trustees election in which
equal preferences were allowed.

To run, for example, the first of these:
`r # library(pref) hc12c=stv(hc12)`

This code produces a list of variables summarising the count and final
outcome. For details and options, see the manual.
