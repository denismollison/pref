# stv.batch - starting from stv18.R - revised 31aug22 to work with SC2022tidied
# prog to run STV on a whole directory of elections (e.g. all Scottish Councils)
#
# choose ("sys=..") meek or wig_5pd as used in Sc Council elections

#' STV election count
#'
#' @param indirec Directory with vote data
# #' @param parties File with party acronyms and colours
#' @param verbose Whether to report on each stage of the count
#' @param plot Whether to produce web pages with plots of countcheck()
#' @param outdirec Directory for webpages (if produced)
#' @param election Name of election
#' @param map Whether to include ward maps (when available)
#' @return A list containing votes and keep vals at each stage, + optional web pages
#' @export
#'
# #' @examples indirec=system.file("extdata","SC2007x",package="pref",mustWork=TRUE)
# #' @examples sc07x=stv.batch(indirec)
#'
#sc22=stv.batch("Scottish Council Election 2022","SC2022tidied","SC2022data",plot=F)

stv.batch=function(election,indirec,outdirec=tempdir(),plot=TRUE,verbose=FALSE,map=FALSE){
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
na=length(areas); areas2=character()
for(ia in 1:na){areas2[[ia]]=paste(strsplit(areas[[ia]]," ")[[1]],collapse="_")}
areas3=areas2   # version for map links
# areas whose name officially includes "City" or "Islands" - ??Eilean Siar
areas3[c(1,7,12,16,23,27)]=c("Aberdeen_City","Dundee_City","City_of_Edinburgh","Glasgow_City","Orkney_Islands","Shetland_Islands")

#  if(parties!=F) ...
writeLines(pties,paste(outdirec,"parties",sep="/"))
mat_all=matrix(0,nrow=na,ncol=(length(pna)+1),dimnames=list(area=areas,party=c(pna,"vacant")))

# main loop or loops - areas then wards
for(ia in 1:na){
 area=areas[[ia]]; area2=areas2[[ia]]
 inarea=paste(indirec,area2,sep="/")
#  wards=readLines(paste(inarea,"wards",sep="/"))   # lines 57- .. edited 31aug22
 wards2=system(paste("ls",inarea),intern=T); df=wards2
 cat(df[1:2],"\n")
nw=length(wards2)
wards=character()  # normal name ("_" -> " ")
 for(iw in 1:nw){
    wards2[[iw]]=strsplit(wards2[[iw]],".blt")[[1]][[1]]
    wards[[iw]]=paste(strsplit(wards2[[iw]],"_")[[1]],collapse=" ")
 }
outarea=paste(outdirec,area2,sep="/")
system(paste("mkdir",outarea))
# writeLines(wards,paste(outarea,"wards",sep="/"))
# not sure still need a file of ward names?
#  if(parties!=F) ...
mat_area=matrix(0,nrow=nw,ncol=(length(pna)+1),dimnames=list(ward=wards,party=c(pna,"vacant")))

for(iw in 1:nw){
ward2=wards2[[iw]]
datafile=paste0(inarea,"/",ward2,".blt")  # or ".dat" ??
electitle=c(paste0('<a href="../../index.html">',election,'</a>'),
paste0('<a href="../index.html">',area,'</a>'))
# wn=as.numeric(strsplit(ward2,"_")[[1]][[1]])

# bit to deal with maps ..
if(map==TRUE){
# add link to map - details specific to SC2022- rethink for general prog ! map=paste0("https://boundaries.scot/Electoral/5th_Reviews/",areas3[[ia]],"/5th_Review_",areas3[[ia]],"_Ward_",wn,".pdf")
# areas with changes from the 2019 Islands Review
if(ia==13){map="https://boundaries.scot/sites/default/files/CNES_SSI_2021_Maps.pdf"}
if(ia==21){map="https://boundaries.scot/sites/default/files/North_Ayrshire_SSI_2021_Maps.pdf"}
if(ia==23){map="https://boundaries.scot/sites/default/files/Orkney_Islands_SSI_2021_Maps.pdf"}
if(ia==27){map="https://boundaries.scot/sites/default/files/Shetland_Islands_SSI_2021_Maps.pdf"}
# areas with individual ward maps missing on BS
if(ia==4){map="https://boundaries.scot/electoral/5th_reviews/argyll_bute_report.pdf"}
if(ia==7){map="https://boundaries.scot/electoral/5th_reviews/dundee_city_report.pdf"}
if(ia==26){map="https://boundaries.scot/electoral/5th_reviews/scottish_borders_report.pdf"}
}

# elecdata=pref.data(datafile,mult=TRUE,parties=paste(indirec,"parties",sep="/"),ballot=FALSE,friendly=TRUE)
elecdata=pref.data(datafile,mult=TRUE,parties=paste(indirec,"parties",sep="/"),friendly=TRUE)
elecdata=append(elecdata,list(map=map))
outward=paste0(outarea,"/",ward2)
save(elecdata,file=paste(outarea,paste0(ward2,".rda"),sep="/"))    # edited 31aug22
datafile=paste(inarea,df[[iw]],sep="/")
cat(datafile,"\n")
# system(paste("cp",datafile,paste0(outarea,"/",datafile.blt)))
system(paste("cp",datafile,outarea))
ed=elecdata
cands=cbind(ed$f,ed$n,ed$p)   # ; votes=as.character(cbind(ed$m,ed$v))
# save in friendly ballot format
sink(paste0(outward,".dat"))
cat(ed$e); cat("\n")
cat(ed$s,ed$c,ed$nv,sum(ed$m)); cat("\n")
for(i in 1:ed$c){cat(paste0(ed$f[[i]]," ",ed$n[[i]],",",ed$p[[i]])); cat("\n")}
if(ed$nv>0){for(i in 1:ed$nv){cat(ed$m[[i]],ed$v[i,]); cat("\n")}}
sink(NULL)
if(plot==TRUE){system(paste("mkdir",outward))}
# now run stv.wig, with webpages edited to include map
# cat(ed$e,ed$nv,"\n")
if(ed$nv>0){
ewig=stv.wig(elecdata,outdirec=outward,electitle=electitle,map=map,verbose=F,plot=F)
x=ewig$i[length(ewig$i)][[1]]; elec=x[x>0]
}else{
# uncontested case ..
cat("stv.batch line 104 - uncontested\n")
elec=1:ed$c
result=paste("Elected (uncontested):",elec,sep="  ")
ewig=list(elec=result,itt=elec,votes=NA,va=NA)
# if plot=T make webpages to go with vote plots, and if verbose=T display them
if(plot==T){wp=webpages(elecdata,va=NA,vo=NA,q0=NA,itt=NA,outward,sys="wig",map,electitle)
if(verbose==T){if(ed$nv>0){grDevices::dev.off()}
    utils::browseURL(wp[[1]],browser="open")}}
}
save(ewig,file=paste(outarea,paste0(ward2,"_wig.rda"),sep="/"))    # edited 31aug22
elecp=ed$p[elec]
eleci=nparty(elecp,pna)
x=table(eleci)
y=as.numeric(names(x))
vec=rep(0,length(pna))
vec[y]=x
mat_area[iw,]=c(vec,(ed$s-length(elec)))
}
 cat("end of ",area,"\n")
if(plot==TRUE){webpage_area(outarea,area,election,mat_area,wards2)}
mat_all[ia,]=apply(mat_area,2,sum)
}
# make webpages at top level
webpage_election(outdirec,election,mat_all,areas,areas2,pn,pna)
}
