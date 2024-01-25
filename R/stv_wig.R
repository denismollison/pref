# stv.wig(() - last revised 25 jan 2024

#' STV election count using WIG as for Scottish Council elections
#' calculated to 5 places of decimals as used for those elections
#'
#' @param votedata File with vote data
#' @param outdirec Needs to be set for permanent record of results
#' @param verbose If =TRUE reports and pauses at each stage of the count
#' (press return to continue to next stage)
#' @param plot If =TRUE (default) produces plots of count and webpages in outdirec
#' @param webdisplay If =TRUE displays plots and statistics as web pages
#' @param map Link to a map or other URL associated with election
#' @param timing Whether to report computing time at each stage
#'
#' @return A list containing votes at each stage, + optional web pages; for details see manual pref_pkg_manual.pdf (section 3)
#' @export
#'
#' @examples hc12wig=stv.wig(hc12,plot=FALSE)
#' @examples nws17wig=stv.wig(nws17,plot=FALSE)
#' @examples p17wig=stv.wig(p17,plot=FALSE)
#' @examples cnc17wig=stv.wig(cnc17,plot=FALSE)
#'
stv.wig=function(votedata,outdirec=tempdir(),verbose=FALSE,plot=TRUE,webdisplay=FALSE,timing=FALSE,map=FALSE){
# don't try plotting if package jpeg is not available:
if(requireNamespace("jpeg")==FALSE){
plot=FALSE; warning("package jpeg not available, setting plot=FALSE")
}
sys="wig"
tim0=proc.time()    # to track computing time taken (use timing=TRUE to print for each stage)
# read and unpack elecdata - only essential component is vote matrix ed$v
vd=votedata; vote=vd$v
nvd=names(vd)
if("s" %in% nvd){ns=vd$s}else{ns=as.numeric(readline("number of seats? "))}
nv0=dim(vote)[[1]]; nc0=dim(vote)[[2]]
if("e" %in% nvd){elecname=vd$e}else{elecname="election"}
if("c" %in% nvd){nc=vd$c}else{nc=nc0}
if("nv" %in% nvd){nv=vd$nv}else{nv=nv0}
if("m" %in% nvd){mult=vd$m}else{mult=rep(1,nv)}
totalvotes=sum(mult); na2=dimnames(vote)[[2]]
if(is.null(na2)){na2=let(nc)}else{if(na2[[1]]=="V1"){na2=let(nc)}}
if("n" %in% nvd){name=vd$n}else{if("n2" %in% nvd){name=vd$n2}else{name=na2}}
if("n2" %in% nvd){name2=vd$n2}else{name2=name}
if("f" %in% nvd){fname=vd$f}else{fname=rep("",nc)}
if("p" %in% nvd){party=vd$p}else{party=rep("",nc)}; np={party[[1]]==""}
if("col" %in% nvd){colour=vd$col}else{colour=grDevices::rainbow(nc)}
vd=list(e=elecname,s=ns,c=nc,nv=nv,m=mult,v=vote,f=fname,n=name,n2=name2,p=party,col=colour); votedata=vd

qa=ceiling((totalvotes+1)/(ns+1))  # quota (fixed)
qpc=100*qa/totalvotes

# print election name and basic statistics
cat("Election: ",elecname,"\n")
cat("System: WIG STV\n")
cat("To fill",ns,"seats; ",nc," candidates:\n")
cat(paste(name,collapse=", ")); cat("\n")
cat(totalvotes,"votes;  quota",qa); cat("\n\n")

# quota (fixed) and quota (fixed) andhousekeeping variables
inn=rep(0,nc)	# 0 indicates still in play (-1 elim, 1 elec, 2 elec & transf)
ic=1:nc
ne=0
it=numeric(); ite=numeric(); backlog=numeric(); ann=rep(0,nc); elec=numeric()
iter=0			# keeps track of number of iterations in count
st=character()          # ? for stages
csum=numeric()          # count summary (votes at each stage); note also vm, va
dnext=""                # text carried over from one stage to next
txt=character()         # text describing decisions at each stage
itt=list()              # cand nos in order of elec/excl for each stage
trf=c("","t")
if(!dir.exists(outdirec)){dir.create(outdirec)}

# calculate first preferences
f=numeric()
ff=rep(0,nc)
for(iv in 1:nv){
 b=vote[iv,]
 if(max(b)>0){
  f0=min(b[b!=0]); fp=ic[b==f0]
  f[[iv]]=fp
  ff[[fp]]=ff[[fp]]+mult[[iv]]
}}
stage=0
vm=matrix(0,nrow=nc,ncol=nc+1)   # changed!
rem=rep(1,nv)

# transfer step - start of specifically wig stuff
while(ne<ns){   # start of main loop (`while no. elec < no. of seats')
 iter=iter+1
 done=length(it)
 stage=stage+1
 vm[,inn!=2]=0

# count votes
 for(iv in 1:nv){
  b=vote[iv,]
  if(max(b)>0){
   f0=min(b[b!=0]); fp=ic[b==f0]
   vm[f[[iv]],fp]=vm[f[[iv]],fp]+mult[[iv]]*rem[[iv]]
 }}
 vm[,nc+1]=ff-apply(vm,1,sum)
 v=apply(vm,2,sum)
 vmp=vm   # save values of vm for plotting at end of stage
 csum=cbind(csum,v); st=c(st,paste("stage",stage,sep=""))
 j=ic[inn[ic]==0 | inn[ic]==1]
 je=j[v[j]>=qa]      # any still in with >=quota deemed elected
 if(length(je)>0){
  je=je[order(-v[je])]
  inn[je]=1
  if(length(je)>1){backlog=je[2:length(je)]}else{backlog=numeric()}   # the `queue'
 }
# if only ns candidates left, declare all elected
 act=ic[inn[ic]>=0]
 if(length(act)==ns){
  i2=ic[inn[ic]==1]; it=c(it,i2[order(-v[i2])])
  i3=ic[inn[ic]==0]; it=c(it,i3[order(-v[i3])])
  inn[act]=max(inn[act],1)
 }else{
  if(length(inn[inn>0])==ns){
   i2=ic[inn[ic]==1]; it=c(it,i2[order(-v[i2])])
  }else{
# decide whether surplus or elimination next
  if(length(je)>0){
   jm=je[[1]]   # deal for now with largest of these
   inn[[jm]]=2
   tr=1-qa/v[[jm]]; it=c(it,jm)
   vm[,jm]=vm[,jm]*(1-tr)
   }else{   # otherwise exclude lowest
   if(length(act)==ns+1){
    io=order(-v[ic]); il=length(it[it>0])
    if(il<ns){
     jp=io[ns]; jm=io[ns+1]
     margin=v[jp]-v[jm]
     it=c(it,io[(il+1):ns]); inn[io[(il+1):ns]]=1; je=io[(il+1):ns]
    }
# to add those `elected with <q' to elec
   }else{
    ix=ic[inn[ic]==0]; lx=length(ix); io=order(-v[ix])
    jp=ix[io[lx-1]]
    jm=ix[io[lx]]; margin=v[jp]-v[jm]
    tr=1; it=c(it,-jm)
    inn[jm]=-1
    }
# can do extra output to identify close contests - qv R2019 version if wanted
}
# adjust vote file
  for(i in 1:nv){
   b=vote[i,]
   if(max(b)>0){
    bm=min(b[b!=0]); fp=ic[b==bm]
    if(fp==jm){rem[[i]]=rem[[i]]*tr; rem[[i]]=.00001*floor(rem[[i]]*(10^5))}
    vote[i,]=vote[i,]*{inn[1:nc]==0}
    if(inn[[fp]]==1){vote[i,fp]=1}
   }}
 }}

 ne=length(ic[inn[ic]>0])
 ne2=length(it[it>0])

 it_new=it[(done+1):length(it)]
 ite=it_new[it_new>0]
 xcl=it_new[it_new<0]
# additions to plotting text are for non-transferable
 je=je[ann[je]==0]
 elec=c(elec,je)
 if(stage==1){
  va=vm; itt=list(it)
 }else{
  va=array(c(va,vm),dim=c(nc,(nc+1),stage))
  itt=append(itt,list(it))
 }

# write output text
 if(ne==ns){final=" - final result"}else{final=""}
 dec2=""
 if(stage==1){dec1=paste("first preferences -",final,sep="")}else{
  dec1=paste("stage ",stage,final," - ",dnext,sep="")
 }
 if(length(ite)>0){
  if(length(je)>0){
   enames=name2[je]; x=plural(enames)
   dec2=paste(x$out,x$has,"achieved the quota, so",x$is,"elected")
  }
  x=plural(name2[ite])
  dnext=paste("after transfer of surplus",x$es," of ",x$out,sep="")
 }
 if(length(ite)==0){
  if(ne==0){nomore="no-one"}else{nomore="no-one else"}
  dec2=paste(nomore,"has achieved the quota, so exclude",name2[abs(xcl)])
  dnext=paste("after transfer of votes of",name2[abs(xcl)])
 }
# correction for when last elected don't reach quota
 if(final != ""){
  if(v[[ite[[1]]]]<qa){
            dec2=paste(name2[[jm]],"is excluded, so")
   enames=name2[ite]; x=plural(enames)
   dec2=paste(dec2,x$out,x$is,"elected")
 }}
 dec=c(dec1,dec2)
 txt=cbind(txt,dec)
 tim=proc.time()-tim0;  pt=tim[[1]]

# make permanent plots of stage (if plot=TRUE)
 if(plot==TRUE){
  wi=(nc+4.5); w=wi*120   # plot width in (approx) inches, and in pixels
  for(i in 2:1){
   transf=i-1
   plotfile=paste(outdirec,paste("stage",trf[[i]],stage,".jpg",sep=""),sep="/")
   h=600+200*transf
   grDevices::jpeg(plotfile,width=w,height=h)
  voteplot(ns,vmp,qpc,it,dec,name2,party,colour,transf,elecname,sys="wig")
   grDevices::dev.off()
  }}

if(timing==TRUE){cat(stage,"    process time ",pt," secs    "); cat("\n")}
# print decision (if verbose=TRUE)
if(verbose==TRUE){
cat(dec,sep="\n"); cat("\n")
# .. and plot current state of votes if plot=TRUE
  if(plot==TRUE){plot_jpeg(plotfile,stage)}
  if(final==""){readline("next? ")}
 }
ann[je]=1
}          # closes stages loop, i.e. end of this election count

csum[nc+1,]=totalvotes-apply(as.matrix(csum[1:nc,]),2,sum)   # to change
# fudge to get "non-transferred" into line

cname=name
if(length(fname[fname!=""])>0){cname=paste0(cname,", ",fname)}
if(length(party[party!=""])>0){cname=paste0(cname," (",party,")")}
dimnames(csum)=list(name=c(cname,"non-transferable"),stage=st)

elec=it[it>0]; x=elec
txt=matrix(txt,nrow=2)
qtxt=paste0("Total votes ",totalvotes,",  quota = ",qa)
pp=paste(" (",party[x],")",sep=""); if(pp[[1]]==" ( )"|pp[[1]]==" ()"){pp=""}
elected=paste(fname[x]," ",name[x],pp,sep="",collapse=", ")
cat("\n"); cat(paste0("Those elected, in order of election:\n"))
cat(elected); cat("\n\n")

if(verbose==TRUE){cat("\nVotes at each stage:\n")
    print(round(csum,2))
    cat("\n",qtxt,"\n")
}

# save result details and elecdata in R data files
countdata=list(sys="wig",el=elected,itt=itt,ctext=txt,csum=csum,qtext=qtxt,va=va)
elecdata=c(votedata,countdata)
elecfile=paste(strsplit(elecname," ")[[1]],collapse="_")
save(elecdata,file=paste0(outdirec,"/",elecfile,"_",sys,".rda"))
# save(votedata,file=paste0(outdirec,"/",elecfile,".rda"))
   
# if plot=TRUE make webpages to go with vote plots, ..
# .. and if verbose=TRUE and webdisplay=TRUE display them
if(plot==TRUE){
 wp=webpages(elecdata,outdirec,map)
#  if(verbose==TRUE){grDevices::dev.off()}
 if(webdisplay==TRUE){utils::browseURL(wp[[1]],browser="open")}
}
elecdata
}
