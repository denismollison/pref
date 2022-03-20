
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stv

<!-- badges: start -->
<!-- badges: end -->

There’s a full manual stv_pkg_manual.pdf

The goal of stv is to count votes from an STV election, together with
clear explanatory graphics. Its core option is Meek STV, which is the
purest expression of <a href="https://www.macs.hw.ac.uk/~denis/stv/">the
simple principles of STV</a>, but requires electronic counting. It can
handle votes expressing equal preference for some of the candidates.

You can install the development version of stv from Github
`r # install.packages("devtools") install_github("denismollison/stv")`

Three examples are provided, drawn from real elections, hc12, p17 and
j02. Two are from Scottish Council elections, one a very simple case and
the second a more usual one. The third example is from a charity’s
trustees election in which equal preferences were allowed. To run the
first of these: `r # library(stv) hc12c=stv.count(hc12)`

This code produces a list of variables summarising the count and final
outcome. With the default options `verbose=T' and`plot=T’ it also
produces graphics as the count progresses, and embeds these in web pages
when it finishes. The directory for the web pages can be specified by
\`outdirec=directory’; the default is to write them to a temporary
directory.
