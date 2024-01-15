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
ct=outlist$ctext; nst=dim(ct)[[2]]
options(width=max(c(120,nchar(ct))))
cat("\nElection: ",outlist$e,"\n",sep="")
cat("System:",outlist$sys,"STV\n",sep=" ")
cat("To fill",outlist$s,"seats\n",sep=" ")
cat("Candidates: ",paste(outlist$n2,collapse=", "),"\n",sep="")
cat(sum(outlist$m),"votes\n\n",sep=" ")
# count text:
for(i in 1:nst){
cat(ct[1,i]);cat("\n")
cat("  ",ct[2,i]); cat("\n\n")
}
cat("Those elected, in order of election:\n")
cat(outlist$el); cat("\n\n")
cat("Votes at each stage")
if(sys=="meek"){cat(" and final keep values:")}
cat("\n")
print(round(outlist$csum,2)); cat("\n")
cat(outlist$qtext)
}
