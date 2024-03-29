% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stv.R
\name{stv}
\alias{stv}
\title{STV election count - uses Meek STV, allows equal preferences}
\usage{
stv(
  votedata,
  outdirec = tempdir(),
  plot = TRUE,
  webdisplay = FALSE,
  interactive = FALSE,
  messages = TRUE,
  timing = FALSE,
  map = FALSE
)
}
\arguments{
\item{votedata}{File with vote data}

\item{outdirec}{Needs to be set for permanent record of results}

\item{plot}{If =TRUE (default) produces plots of count and webpages in outdirec}

\item{webdisplay}{If =TRUE displays plots and statistics as web pages}

\item{interactive}{If =TRUE reports and pauses at each stage of the count
(press return to continue to next stage)}

\item{messages}{If=TRUE prints 1-line initial and final reports}

\item{timing}{Whether to report computing time at each stage}

\item{map}{Link to a map or other URL associated with election}
}
\value{
A list containing vote and count data, + optional web pages; for details see manual pref_pkg_manual.pdf (section 3)
}
\description{
STV election count - uses Meek STV, allows equal preferences
}
\examples{
cnc17meek=stv(cnc17,plot=FALSE)
c99result=stv(c99,plot=FALSE)
y12meek=stv(y12,plot=FALSE)

}
