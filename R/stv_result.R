# stv.result - text of main statistics of election count - 16jan24
# 16 jan 2024, last revised 23 jan 2024

#' Prints main details of result of an STV election
#'
#' @param elecdata An R list of results from stv or stv.wig
#' @param outfile Where to print results (terminal or file path)
#' @return Prints out results
#' @export
#'
#' @examples c99result=stv(c99)
#' @examples stv.result(c99result)
#' @examples nws17wig=stv.wig(nws17)
#' @examples stv.result(nws17wig)

stv.result=function(elecdata,outfile="terminal"){

ed=elecdata
elecname=ed$e
ns=ed$s
nc=ed$c
mult=ed$m
name=ed$n
sys=ed$sys
ct=ed$ctext; nst=dim(ct)[[2]]
csum=ed$csum
qtext=ed$qtext
if(sys=="meek"){keep=ed$keep}

options(width=max(c(120,nchar(ct))))

if(outfile!="terminal"){sink(outfile)}
    
cat("\nElection: ",ed$e,"\n",sep="")
cat("System:",sys,"STV\n",sep=" ")
cat("To fill",ns,"seats; ",ed$c,"candidates:\n")
cat(paste(name,collapse=", ")); cat("\n")
totalvotes=sum(mult); q0=totalvotes/(ns+1)
cat(totalvotes,"votes;  initial quota",round(q0,2)); cat("\n\n")

# count text:
for(i in 1:nst){
cat(ct[1,i]);cat("\n")
cat("  ",ct[2,i]); cat("\n\n")
}
cat("Those elected, in order of election:\n")
cat(ed$el); cat("\n\n")

if(sys=="meek"){
ic=1:nc; nst=dim(keep)[[2]]
ir=ic[keep[ic,nst]==100]
if(length(ir)>0){rname=dimnames(csum)[[1]][ir]}
cat("Runner-up:",paste(rname,collapse=", ")); cat("\n\n")
}

cat("Votes at each stage")
if(sys=="meek"){cat(" and final keep values:")}
cat("\n")
print(round(csum,2)); cat("\n")
cat(qtext)

if(outfile!="terminal"){sink()}
}
