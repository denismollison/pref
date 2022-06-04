# functions stv.clean and clean to tidy up large sets of vote data
# likely to be needed before running stv.batch
# 
# stv.clean - as at 14may2022
# function to clean up a whole directory of elections (e.g. all Scottish Councils)
# sorting out variations in ward and party names, and reformatting
# but not doing any analysis or major format change (esp. pref -> ballot)
# crucial step is `dat2=clean(dat,iw,indirec)'
# stv.clean is fairly robust, but ..
# the function clean is very specific to the kinds of errors and complications
# of a specific data set, so will a.s. need changing or replacing for other elections

stv.clean=function(indirec,outdirec="clean",verbose=T){

system(paste("mkdir",outdirec))

# read index files - more ambitiously could work out from directory contents
areas=readLines(paste(indirec,"areas",sep="/"))
writeLines(areas,paste(outdirec,"areas",sep="/"))
na=length(areas)    

# main loops - areas then wards
for(ia in 1:na){
area=areas[[ia]]
area2=paste(strsplit(area," ")[[1]],collapse="_")
inarea=paste(indirec,area2,sep="/")
wards=system(paste("ls",inarea),intern=T)
nw=length(wards)
outarea=paste(outdirec,area2,sep="/")
system(paste("mkdir",outarea))

for(iw in 1:nw){
dat=readLines(paste0(inarea,"/",wards[[iw]]))
dat2=clean(dat,iw,indirec)
datafile=paste(strsplit(dat2[[1]]," ")[[1]],collapse="_")
outfile=paste0(outarea,"/",datafile,".blt")
cat(outfile,"\n\n")
if(verbose==TRUE){cat(ia,iw,"cleaned -",outfile,"\n")}
writeLines(dat2,outfile)
}
# writeLines(wards,paste(outarea,"wards",sep="/"))   # file with list of wards
}}



clean=function(dat,iw,indirec){
# version for 2022 Scottish Council election - as at 14may2022

# input file with variations of party names used in election
pv=readLines(paste(indirec,"party_names",sep="/"))
npv=length(pv); long=character(); short=character()
for(i in 1:npv){
x=strsplit(pv[[i]],"\t")[[1]]
short[[i]]=x[[1]]
if(length(x)==1){long[[i]]=""}else{long[[i]]=x[[2]]}
}
# split data into components
x=as.numeric(strsplit(dat[[1]]," ")[[1]])
nc=x[[1]]; ns=x[[2]]
i=1:length(dat)
nv=i[substring(dat[i],1,1)=="0"]-2
vdata=2:(nv+2)
cdata=(nv+3):(nv+nc+2)
nl=nv+nc+3
dat[[1]]=paste(ns,nc,nv)

# standardize ward names - get rid of ", Ward, -; add no. iw if missing
options(warn=-1)
wn=dat[[nl]]
z=strsplit(wn,"\"")[[1]]
if(length(z)>1){wn=z[[2]]}
z=strsplit(wn,"Ward ")[[1]]
if(length(z)>1){wn=z[[2]]}
wn=strsplit(wn," Ward")[[1]][[1]]
z=strsplit(wn," ")[[1]]   # z[[1]] should be ward number
if(is.na(as.numeric(z[[1]]))){z=c(iw,z)}else{z[[1]]=as.numeric(z[[1]])}
dat[[nl]]=paste(z[z!="-"],collapse=" ")
options(warn=0)
# reduce 54 variant names to 27 actual parties, and use their acronyms
npn=length(short); ip=1:npn
pty=character()
for(i in cdata){
  x=strsplit(dat[[i]],"\"")[[1]]
  j=ip[long[ip]==x[[4]]]
  if(length(j)==0){cat(i,x[[4]],"\n")   # print any party names not yet on file
}else{pty=short[[j]]
dat[[i]]=paste(x[[2]],pty,sep=",")}
}
# output cleaned data - change order to friendly
# (change this line to simply `dat' to leave order unchanged)
dat[c(nl,1,cdata,vdata)]
}
