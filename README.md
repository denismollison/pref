
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
expressing equal preference for some of the candidates.

You can install the development version of pref from Github
`r # install.packages("devtools") install_github("denismollison/pref")`

Five examples are provided, drawn from real elections, hc12, p17, nws17,
yale and j02. Three are from Scottish Council elections, one a very
simple case and the second a more usual one. The third example (nws17)
is one where using the official counting method (stv.wig) no candidate
achieves the fixed quota. The fourth, from a Yale faculty election, is
an example with a large number of candidates (44 for 4 places). A
simplified version, showing just the later stages of this election is
provided as y12.

The final example, j02, is from a charity’s trustees election in which
equal preferences were allowed.

To run the first of these: `r # library(pref) hc12c=stv(hc12)`

This code produces a list of variables summarising the count and final
outcome. With the default options `verbose=T' and`plot=T’ it also
produces graphics as the count progresses, and embeds these in web pages
when it finishes. The directory for the web pages can be specified by
\`outdirec=directory’; the default is to write them to a temporary
directory.
