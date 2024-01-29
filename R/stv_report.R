# stv.report - textreport of main statistics of election count -
# 16 jan 2024 as stv.result; rewritten to avoid written output 27 jan 2024

# Prints main details of result of an STV election


stv.report=function(elecdata){

ed=elecdata
elecname=ed$e
ns=ed$s
nc=ed$c
mult=ed$m
name=ed$n
sys=ed$sys
ct=ed$narrative; nst=dim(ct)[[2]]
csum=ed$count
qtext=ed$quotatext
if(sys=="meek"){keep=ed$keep}

report=""
report=c(report,paste("Election:",elecname))
report=c(report,paste("System:",sys,"STV"))
report=c(report,paste("To fill",ns,"seats; ",nc,"candidates:"))
report=c(report,paste(name,collapse=", "))
totalvotes=sum(mult)
if(sys=="meek"){q0=totalvotes/(ns+1); q0text="votes;  initial quota"}else{
  q0=1+floor(totalvotes/(ns+1)); q0text="votes;  quota"}
report=c(report,paste(totalvotes,q0text,round(q0,2)),"")

# count text:
for(i in 1:nst){
report=c(report,paste(ct[1,i]),paste("  ",ct[2,i]),"")
}
report=c(report,paste("Those elected, in order of election:"),paste(ed$el),"")

if(sys=="meek"){
ic=1:nc; nst=dim(keep)[[2]]
ir=ic[keep[ic,nst]==100]
if(length(ir)>0){rname=dimnames(csum)[[1]][ir]}
report=c(report,paste("Runner-up:",paste(rname,collapse=", ")))
report=c(report,"")
}

if(sys=="meek"){
report=c(report,paste(("Votes at each stage and final keep values:")))
}else{
report=c(report,paste("Votes at each stage:"))
}

csvec=utils::capture.output(
  print(round(csum,2),collapse="\n")
)

report=c(report,csvec,"",paste(qtext))

report
}
