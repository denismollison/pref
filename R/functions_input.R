# stvfunctions_input.R
# main function stv.data
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
#' @param format Default "ballot"
#' @param details Whether full election detail (default) or just vote matrix
#'
#' @return A standardised list of election info to save in a .rda file
#' @export
#'
# @examples p17=stv.data("inst/extdata/Partick2017.blt",mult=T,parties="inst/extdata/parties_SC2017.txt")

stv.data=function(datafile,mult=F,parties=F,format="ballot",details=T){

# minimal case where data are just a vote matrix with header of candidate names
if(details==F){     # minimal case with abbrev names and vote matrix only
vote=as.matrix(utils::read.table(datafile,header=T,row.names=NULL,sep=" "))
vote[vote==""]=0
  if(mult==T){mult=vote[,1]; vote=vote[,2:dim(vote)[[2]]]}else{
  mult=rep(1,dim(vote)[[1]])}
nv=sum(mult); nc=dim(vote)[[2]]; name2=dimnames(vote)[[2]]
fname=rep("",nc); name=name2; party=rep("",nc)
colour=grDevices::rainbow(nc)
elecname=readline("election name?")
ns=as.numeric(readline("number to elect?"))
}else{

dat=base::readLines(datafile)
elecname=dat[[1]]
x=as.numeric(strsplit(dat[[2]]," ")[[1]])
ns=x[[1]]; nc=x[[2]]; ic=1:nc
    name=character(); fname=character(); party=rep("",nc)
    colour=grDevices::rainbow(nc)
for(i in ic){
    x=strsplit(dat[[2+i]],",")[[1]]
    if(length(x)>1){party[[i]]=x[[2]]}
y=strsplit(x[[1]]," ")[[1]]; z=length(y)
    name[[i]]=y[[z]]
    if(z==1){fname[[i]]=""}else{fname[[i]]=paste(y[1:(z-1)],collapse=" ")}
}
name2=abbrev(name,fname)
ip=ic[party!=""]; cat(parties,"\n\n")
if(length(ip)>0){if(parties!=F){
    colour=rep("white",nc)
    colour[ip]=party_colour(party[ip],parties)}
    else{cat("recommend re-run with party colours file if available\n\n")}
}
nv=length(dat)-nc-2
if(nv>0){
# read data as a matrix
    n2=length(strsplit(dat[[nc+3]]," ")[[1]])
vote=matrix(0,nrow=nv,ncol=n2)
    for(i in 1:nv){
        vote[i,]=as.numeric(strsplit(dat[[nc+2+i]]," ")[[1]])
    }
    if(n2>nc){
        mult=vote[,1]; vote=vote[,2:n2]
    }else{mult=rep(1,nv)}
}else{m=0; v=0}   # uncontested case, recognised by nv=0
}
list(e=elecname,s=ns,c=nc,nv=nv,m=mult,v=vote,f=fname,n=name,n2=name2,p=party,col=colour)
}
# save(d,"elec.R")    # load(d,"elec.R")


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
cat("warning - duplicate surname -",dn,"\n",fname[dnc],"\n")
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
}


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
