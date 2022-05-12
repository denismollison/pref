# stv.batch - starting from stv18.R
# prog to run STV on a whole directory of elections (e.g. all Scottish Councils)
# 
# choose ("sys=..") meek or wig_5pd as used in Sc Council elections

#' STV election count
#'
#' @param indirec Directory with vote data
#' @param parties File with party acronyms and colours
#' @param verbose Whether to report on each stage of the count
#' @param plot Whether to produce web pages with plots of countcheck()
#' @param outdirec Directory for webpages (if produced)
#' @param election Name of election
#'
#' @return A list containing votes and keep vals at each stage, + optional web pages
#' @export
#'
# #' @examples indirec=system.file("extdata","SC2007x",package="stv",mustWork=TRUE)
# #' @examples sc07x=stv.batch(indirec,parties=TRUE)
#' 

stv.batch=function(election,indirec,parties=F,outdirec=tempdir(),verbose=T,plot=F){
#  version with parties for SC elecs - labelled party bits "# if(parties!=F) ..."
#  tim0=proc.time()    # to track computing time taken

# sys="meek"
sys="wig"

system(paste("mkdir",outdirec))
    
# provides party names & colours, and function to link them
#  if(parties!=F) ...
pties=readLines(paste(indirec,"parties",sep="/"))
np=length(pties)
pna=character(); pcolour=character(); pn=character()
for(i in 1:np){
z=strsplit(pties[[i]],"\t")[[1]]
pna[[i]]=z[[1]]; pcolour[[i]]=z[[2]];pn[[i]]=z[[3]]
}

areas=readLines(paste(indirec,"areas",sep="/"))
na=length(areas)    
areas2=readLines(paste(indirec,"areas2",sep="/"))
writeLines(areas2,paste(outdirec,"areas",sep="/"))
    
#  if(parties!=F) ...
writeLines(pties,paste(outdirec,"parties",sep="/"))
mat_all=matrix(0,nrow=length(areas),ncol=length(pna),dimnames=list(area=areas,party=pna))
    
# main loop or loops - areas then wards
for(ia in 1:na){
 area=areas[[ia]]
 area2=areas2[[ia]]

inarea=paste(indirec,area2,sep="/")
 wards=readLines(paste(inarea,"wards",sep="/"))
 nw=length(wards);  wards2=character()
 for(iw in 1:nw){wards2[[iw]]=paste(c(iw,strsplit(wards[[iw]]," ")[[1]]),collapse="_")}
#  system(paste("cp parties",inarea))
outarea=paste(outdirec,area2,sep="/")
system(paste("mkdir",outarea))
writeLines(wards,paste(outarea,"wards",sep="/"))
#  if(parties!=F) ...
mat_area=matrix(0,nrow=nw,ncol=length(pna),dimnames=list(ward=wards,party=pna))

for(iw in 1:nw){
ward2=wards2[[iw]]
datafile=paste0(inarea,"/",ward2,".dat")
# cat(datafile,"\n")
# add link to map - very specific - rethink for general prog !
rev=4
electitle=c(paste0('<a href="../../index.html">',election,'</a>'),
paste0('<a href="../index.html">',area,'</a>'))
#  if(parties!=F) ...
map=paste0("https://boundaries.scot/Electoral/",rev,"th_Reviews/",area2,"/",rev,"th_Review_",area2,"_Ward_",iw,".pdf")

cat(ward2,"\n")
elecdata=stv.data(datafile,mult=TRUE,parties=paste(indirec,"parties",sep="/"),ballot=FALSE,friendly=TRUE)
elecdata=append(elecdata,list(map=map))
outward=paste0(outarea,"/",ward2)

save(elecdata,file=paste0(outward,".rda"))
system(paste("cp",datafile,outarea))
ed=elecdata
cands=cbind(ed$f,ed$n,ed$p); votes=as.character(cbind(ed$m,ed$v))
# save in friendly ballot  format
sink(paste0(outward,".txt"))
cat(ed$e); cat("\n")
cat(ed$s,ed$c,ed$nv,sum(ed$m)); cat("\n")
for(i in 1:ed$c){cat(paste0(ed$f[[i]]," ",ed$n[[i]],", ",ed$p[[i]])); cat("\n")}
for(i in 1:ed$nv){cat(ed$m[[i]],ed$v[i,]); cat("\n")}
sink(NULL)

system(paste("mkdir",outward))
# now run stv.wig, with webpages edited to include map
ewig=stv.wig(elecdata,outdirec=outward,electitle=electitle,map=map,verbose=F)
save(ewig,file=paste(outward,paste0(ward2,"_wig.rda"),sep="/"))

x=ewig$i[length(ewig$i)][[1]]; elec=x[x>0]
elecp=elecdata$p[elec]
eleci=nparty(elecp,pna)
x=table(eleci)
y=as.numeric(names(x))
vec=rep(0,length(pna))
vec[y]=x
mat_area[iw,]=vec

}
webpage_area(outarea,area,election,mat_area,wards2)
mat_all[ia,]=apply(mat_area,2,sum)
}

# make webpages at top level
webpage_election(outdirec,election,mat_all,areas,areas2,pn,pna)  
# warddirec=paste(outdirec,fward,sep="/")
# need to make direc
# result=stv(elecdata,warddirec=tempdir(),verbose=F,plot=T)
# stv should make ward webpage - need to give it flexible title variable
# check elec details at bottom lhs of plots

}
