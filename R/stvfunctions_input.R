# stvfunctions_input.R
# main function votedata; with abbrev, party_colour and capwords for tidying up

# function votedata reads STV data with full header
# election title, ns, nc
# candidate info: fname,name; and party
# full vote data matrix: vote, mult
# if nc+1 columns, assumes mult = 1st column, otherwise mult=1
# data in ballot format (if in pref order, use pref2blt.R to convert)
votedata=function(datafile){
dat=readLines(datafile)    # if(dat[[1]]!=ward){cat("Wrong ward name?","\n")}
x=as.numeric(strsplit(dat[[2]]," ")[[1]])
ns=x[[1]]; nc=x[[2]]; ic=1:nc
# datafile may contain x[3:4]=c(nv,totalvote) - could be used as check
name=character(); fname=character(); party=rep("",nc); colour=character()
for(i in ic){
    x=strsplit(dat[[2+i]],",")[[1]]
    if(length(x)>1){party[[i]]=x[[2]]}
y=strsplit(x[[1]]," ")[[1]]; z=length(y)
    name[[i]]=y[[z]]
    if(z==1){fname[[i]]=""}else{fname[[i]]=paste(y[1:(z-1)],collapse=" ")}
}
name2=abbrev(name,fname)
ip=ic[party!=""]
if(length(ip)>0){
    colour=rep("white",nc)
    colour[ip]=party_colour(party[ip],parties)
}else{colour=rainbow(nc)}

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
    d=list(s=ns,c=nc,nv=nv,m=mult,v=vote,f=fname,n=name,n2=name2,p=party,col=colour)
}


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
        cat(elecname,"- duplicate surname -",dn,"\n",fname[dnc],"\n")         # diagnostic of duplicate names
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


# function party_colour() to calculate party colour from party acronyms
# file parties also contains full party names (pn) - not used at mo
party_colour=function(pa,parties){
ps=readLines(parties); np=length(ps)
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
