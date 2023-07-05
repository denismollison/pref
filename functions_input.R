# stvfunctions_input.R - last revised 28 june 2023
# main function pref.data
# uses abbrev, party_colour and capwords for tidying up
# for either .blt format with full details
# or minimal format - vote matrix with candidate names/codes
# election title, ns, nc
# candidate info: fname,name; and party
# full vote data matrix: vote, mult
# data in ballot format (if in pref order, use pref2blt.R to convert)

#' put election data in an R file (.rda)
#'
#' @param datafile File with election data
#' @param mult Whether includes aggregated votes (default F)
#' @param parties File with party details (default F, i.e. omit)
#' @param ballot Default F (meaning pref format)
#' @param friendly Default F (meaning most details after votes)
#' @param details Whether full election detail (default) or just vote matrix
#'
#' @return A standardised list of election info to save in a .rda file
#' @export
#' @examples datafile=system.file("extdata","jmt2002.dat",package="pref")
#' @examples jmt02=pref.data(datafile)
#' @examples datafile=system.file("extdata","Partick2017.blt",package="pref")
#' @examples parties17=system.file("extdata","parties_SC2017.txt",package="pref")
#' @examples p17=pref.data(datafile,mult=TRUE,parties=parties17,ballot=TRUE,friendly=TRUE)
#' # @examples y12=pref.data("inst/extdata/yale12.blt",details=F)

pref.data=function(datafile,mult=F,details=T,parties=F,ballot=F,friendly=F){
#
# first: minimal case - data a vote matrix with header of candidate names
if(details==F){     # minimal case with abbrev names and vote matrix only
vote=as.matrix(utils::read.table(datafile,header=T,row.names=NULL,sep=" "))
vote[vote==""]=0
  if(mult==T){mul=vote[,1]; vote=vote[,2:dim(vote)[[2]]]}else{
  mul=rep(1,dim(vote)[[1]])}
nv=dim(vote)[[1]]; nc=dim(vote)[[2]]; name2=dimnames(vote)[[2]]
fname=rep("",nc); name=name2; party=rep("",nc)
colour=grDevices::rainbow(nc)
elecname=readline("election name?")
ns=as.numeric(readline("number to elect?"))
}else{
# detailed case: details=T - elecname, ns, nc, names - option of parties
dat=base::readLines(datafile)
id=1:length(dat)
name=character(); fname=character(); mul=numeric(); vote=numeric()
if(friendly==T){    # user-friendly file order, with details first, then votes
elecname=dat[[1]]
x=as.numeric(strsplit(dat[[2]]," ")[[1]])
ns=x[[1]]; nc=x[[2]]; ic=1:nc
cdata=2+ic
# what if no contest, therefore no vote data? (need to cover this case for batch mode)
if(ballot==TRUE){nv=length(dat)-(nc+2)}else{nv=length(dat)-(nc+3)}
if(nv>0){vdata=(nc+3):length(dat)}
}else{
# if friendly=F, i.e. data in user-unfriendly file order, with most details at end (Hill's format)
x=as.numeric(strsplit(dat[[1]]," ")[[1]])
nc=x[[1]]; ns=x[[2]]
nv=id[substring(dat[id],1,1)=="0"]-2
if(nv>0){vdata=2:(nv+1)}
cdata=(nv+3):(nv+nc+2)
elecname=dat[[nv+nc+3]]
}
# for either input order, can now extract details and votes
# candidate names, calculate a short-form unique version, name2
# modify for 2022 SC data
party=rep("",nc); fname=rep("",nc); name=rep("",nc)
for(i in cdata){
  j=i+1-cdata[[1]]
  x=strsplit(dat[[i]],",")[[1]]
  if(length(x)>1){
    party[[j]]=x[[2]]
  }
  y=strsplit(x[[1]]," ")[[1]]; z=length(y)
  name[[j]]=y[[z]]
  if(z>1){fname[[j]]=paste(y[1:(z-1)],collapse=" ")}
}
name2=abbrev(name,fname)
# parties and party colours if specified
if(parties!=F){
  ip=ic[party!=""]
if(length(ip)>0){
  colour=rep("white",nc)
  colour[ip]=party_colour(party[ip],parties)}
else{cat("recommend re-run with party colours file if available\n\n")}
}else{colour=grDevices::rainbow(nc)}
# and last but not least - the votes
if(nv>0){
  vote=matrix(0,nrow=nv,ncol=nc)
  if(mult==F){mul=rep(1,nv)}
  if(ballot==T){
    for(iv in 1:nv){
    x=strsplit(dat[[vdata[[iv]]]]," ")[[1]]
    if(mult==T){mul[[iv]]=as.numeric(x[[1]]); x=x[2:length(x)]}
    if(length(x)!=nc){cat("vote ",i," has length ",length(x))}
      vote[iv,]=as.numeric(x)
  }}else{
# if ballot=F
for(iv in 1:nv){
x=strsplit(dat[[vdata[[iv]]]]," ")[[1]]
x=x[x!=""]
nx0=length(x)
mul[[iv]]=as.numeric(x[[1]])  # for pref format assume first element is mult
if(x[[nx0]]!="0"){cat("check failure at vote no. ",iv,"\n")}
if(nx0>2){
  x=x[2:(nx0-1)]; nx=length(x)   # actual vote
pr=1; incr=1; pref=numeric()
for(i in 1:nx){
nch=nchar(x[[i]])
if(substring(x[[i]],1,1)=="("){incr=0; x[[i]]=substring(x[[i]],2,nch)}
if(substring(x[[i]],nch,nch)==")"){incr=1; x[[i]]=substring(x[[i]],1,(nch-1))}
pref[[i]]=pr; pr=pr+incr
}
xn=as.numeric(x)
vote[iv,xn]=pref
}
}}
}else{mul=numeric(); vote=numeric()}   # uncontested case, recognised by nv=0
}
list(e=elecname,s=ns,c=nc,nv=nv,m=mul,v=vote,f=fname,n=name,n2=name2,p=party,col=colour)
}
# end of function prefdata; may want to save result, d say, using`save(d,file="elec.rda")'
# .. and later `load(d,file="elec.rda")'


abbrev=function(name,fname){
# adds name2, abbreviated names for output - need to check don't have 2 same
# for 2012 suffices to have 2 letters of first name, or initials
name2=name; ic=1:length(name)
# check for duplicate names
tn=table(name)
dn=dimnames(tn)$name[tn>1]
if(length(dn)>0){
  for(idn in 1:length(dn)){
  dnc=ic[name==dn[[idn]]]
  fname[dnc]=capwords(fname[dnc],T)
cat("warning - duplicate surname -",dn[[idn]]," (",fname[dnc],")\n")
  tfn=table(fname[dnc])
  kc=0; while(max(table(name2[dnc]))>1 & kc<2){
    kc=kc+1
    for(jc in ic[name==dn[[idn]]]){
    name2[[jc]]=paste(name[[jc]],",",substring(fname[[jc]],1,kc),sep="")
  }}
# but if they also have same first two letters of first name, try initials ..
tnc=table(name2[dnc])
if(max(table(name2[dnc]))>1){
  for(jc in dnc){
  inits=paste(substring(strsplit(fname[[jc]]," ")[[1]],1,1),collapse="")
  name2[[jc]]=paste(name[[jc]],inits,sep=",")
}}
}}
name2
} # end of function abbrev


# party_colour calculates party colour from parties file
# file parties also contains full party names (pn) - not used at mo
party_colour=function(pa,parties){
ps=base::readLines(parties); np=length(ps)
pna=character(); pcolour=character() # ; pn=character()
for(i in 1:np){
z=strsplit(ps[[i]],"\t")[[1]]
pna[[i]]=z[[1]]; pcolour[[i]]=z[[2]] # ; pn[[i]]=z[[3]]
}
colour=character()
i=1:np
n=length(pa)
for(j in 1:n){
colour[[j]]=pcolour[[i[pna==pa[[j]]]]]
}
colour
}


# function capwords - to tidy upper/lower case names
# (useful when collating data from different sources)
capwords=function(s, strict = FALSE) {
cap=function(s) paste(toupper(substring(s, 1, 1)),
  {s=substring(s, 2); if(strict) tolower(s) else s}, sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
