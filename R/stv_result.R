# stv.result - text of main statistics of election count

#' Prints main details of result of an STV election
#'
#' @param outlist An R list of results from stv or stv.wig
#' @return Prints out results
#' @export
#'
#' @examples c99result=stv(c99)
#' @examples stv.result(c99result)
#' @examples nws17wig=stv.wig(nws17)
#' @examples stv.result(nws17wig)

stv.result=function(outlist){
sys=outlist$sys
ct=outlist$counttext; nst=dim(ct)[[2]]
options(width=max(c(120,nchar(ct))))
cat("\nElection: ",outlist$elecname," (",outlist$sys," STV)\n\n",sep="")
# count text:
for(i in 1:nst){
cat(ct[1,i]);cat("\n")
cat("  ",ct[2,i]); cat("\n\n")
}
cat("Those elected, in order of election:\n")
cat(outlist$elec); cat("\n\n")
cat("Votes at each stage")
if(sys=="meek"){cat(" and final keep values:")}
cat("\n")
print(round(outlist$votes,2)); cat("\n")
cat(outlist$quotatext)
}
