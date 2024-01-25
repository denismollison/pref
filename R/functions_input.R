# functions_input.R - last revised 25 jan 2024
# minor functions abbrev, party_colour and capwords
# used by main input function pref.data

abbrev=function(name,fname){
# adds name2, abbreviated names for output - need to check don't have 2 same
# (for 2012 data suffices to have 2 letters of first name, or initials)
name2=name; ic=1:length(name)
# check for duplicate names
tn=table(name)
dn=dimnames(tn)$name[tn>1]
if(length(dn)>0){
  for(idn in 1:length(dn)){
  dnc=ic[name==dn[[idn]]]
  fname[dnc]=capwords(fname[dnc],T)
warning("duplicate surname -",dn[[idn]]," (",fname[dnc],")")
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
# file parties also contains full party names (pn) - not used currently
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
} # end of function party_colour


# function capwords - to tidy upper/lower case names
# (useful when collating data from different sources)
capwords=function(s, strict = FALSE) {
cap=function(s) paste(toupper(substring(s, 1, 1)),
  {s=substring(s, 2); if(strict) tolower(s) else s}, sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


# function let, to give names to anonyous candidates
let=function(nc){
if(nc<27){n=LETTERS[1:nc]}else{
naz=ceiling(nc/26)
x=c("",as.character(2:naz))
n=paste0(LETTERS,rep(x,rep(26,naz)))
n=n[1:nc]
}
n
}
