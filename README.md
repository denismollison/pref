
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pref

<!-- badges: start -->
<!-- badges: end -->

Preference voting

There’s a full manual pref_pkg_manual.pdf

The goal of the function stv is to count votes from an STV election,
together with clear explanatory graphics. Its core option is Meek STV,
which is the purest expression of
<a href="https://www.macs.hw.ac.uk/~denis/stv/">the simple principles of
STV</a>, but requires electronic counting. It can handle votes
expressing equal preferences for subsets of the candidates. A function
stv.wig implementing the Weighted Inclusive Gregory method, as used in
Scottish council elections, is also provided, and with the same options,
as described below.

You can install the development version of pref from Github
`r # install.packages("devtools") install_github("denismollison/pref")`

Five examples are provided, drawn from real elections, hc12, p17, nws17,
yale and j02. The first three are from Scottish Council elections, the
first a very simple case and the second a more usual one. The third
example (nws17) is one where using the official counting method
(stv.wig) no candidate achieves the fixed quota. The fourth, from a Yale
faculty election, is an example with a large number of candidates (44
for 4 places). A simplified version, showing just the later stages of
this election is provided as y12.

The final example, j02, is from a charity’s trustees election in which
equal preferences were allowed.

To run the first of these: `r # library(pref) hc12c=stv(hc12)`

This code produces a list of variables summarising the count and final
outcome. To be able to access these, you need to set “outdirec” to a
permanent file address. If “plot=T”, plots and webpages are also
produced in outdirec. With the option “verbose=T” the function prints
information on the state of the count at each stage, and if “plot=T”
displays them; if “webpages=T” they are displayed at the end of the
count. Note that when “verbose=T” the function pauses at each stage; you
need to press “return” to continue.
