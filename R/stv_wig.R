#' STV election count using WIG as for Scottish Council elections
#' calculated to 5 places of decimals as used for those elections
#'
#' @param elecdata File with vote data
#' @param verbose Whether to report on each stage of the count
#' @param plot Whether to produce web pages with plots of countcheck()
#' @param outdirec Directory for webpages (if produced)
#' @param electitle For web page heading links if appropriate
#' @param map Link to a map or other URL associated with election
#' @param timing Whether to report computing time at each stage
#'
#' @return A list containing votes at each stage, + optional web pages
#' @export
#'
#' @examples nws17w=stv.wig(nws17,plot=FALSE)
#' @examples p17w=stv.wig(p17,plot=FALSE)
#'
stv.wig=function(elecdata,outdirec=tempdir(),map=FALSE,electitle=character(),verbose=F,plot=T,timing=F){
sys="wig"
cat(plot,"\n\n")
tim0=proc.time()    # to track computing time taken (use timing=T to print for each stage)
# read and unpack elecdata
ed=elecdata; elecname=ed$e
ns=ed$s; nc=ed$c; vote=ed$v; nv=ed$nv; mult=ed$m; totalvotes=sum(mult)
name=ed$n; fname=ed$f; name2=ed$n2
party=ed$p; colour=ed$col; np={party[[1]]==""}
qa=ceiling((totalvotes+1)/(ns+1))  # quota (fixed)
qpc=100*qa/totalvotes

# print election name and basic statistics
cat(elecname)
cat(paste("  (",ns," seats, ",nc," candidates, ",totalvotes," votes)",sep="")); cat("\n")
if(verbose==T){cat("quota ",qa); cat("\n\n")}

# quota (fixed) and quota (fixed) andhousekeeping variables
inn=rep(0,nc)	# 0 indicates still in play (-1 elim, 1 elec, 2 elec & transf)
ic=1:nc
ne=0
it=numeric(); ite=numeric(); backlog=numeric(); ann=rep(0,nc); elec=numeric()
iter=0			# keeps track of number of iterations in count
st=character()          # ? for stages
vo=numeric()            # to record votes at each stage; note also vm, va
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
vm=matrix(0,nrow=nc+1,ncol=nc+1)
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

 vm[,nc+1]=c(ff,0)-apply(vm,1,sum)   # assumes no n-t fps
 v=apply(vm,2,sum)
 vmp=vm   # save values of vm for plotting at end of stage
 vo=cbind(vo,v); st=c(st,paste("stage",stage,sep=""))
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
  va=array(c(va,vm),dim=c((nc+1),(nc+1),stage))
  itt=append(itt,list(it))
 }

# write output text
 if(ne==ns){final=" - final result"}else{final=""}
 dec2=""
 if(stage==1){dec1=paste("first preferences",final,sep="")}else{
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
# correction for when last elec don't reach quota
 if(final != ""){
  if(v[[ite[[1]]]]<qa){
            dec2=paste(name2[[jm]],"is excluded, so")
   enames=name2[ite]; x=plural(enames)
   dec2=paste(dec2,x$out,x$is,"elected")
 }}
 dec=c(dec1,dec2)
 tim=proc.time()-tim0;  pt=tim[[1]]

# make permanent plots of stage (if plot=T)
 if(plot==T){
  wi=(nc+4.5); w=wi*120   # plot width in (approx) inches, and in pixels
  for(i in 2:1){
   transf=i-1
   plotfile=paste(outdirec,paste("stage",trf[[i]],stage,".jpg",sep=""),sep="/")
   h=600+200*transf
   grDevices::jpeg(plotfile,width=w,height=h)
  voteplot(ns,vmp,qpc,it,dec,name2,party,colour,transf,elecname,sys="wig")
   grDevices::dev.off()
 }}
if(timing==T){cat(stage,"    process time ",pt," secs    "); cat("\n")}
# print decision (if verbose=T)
 if(verbose==T){
  cat(dec,sep=", "); cat("\n")
# .. and plot current state of votes if plot=T
  if(plot==T){plot_jpeg(plotfile,stage)}
  if(final==""){readline("next? ")}
 }
ann[je]=1
}          # closes stages loop, i.e. end of this election count

vo[nc+1,]=totalvotes-apply(as.matrix(vo[1:nc,]),2,sum)
# fudge to get "non-transferred" into line
if(length(party[party!=""])>0){
 dimnames(vo)=list(name=c(paste(name,", ",fname," (",party,") ",sep=""),"non-transferable"),stage=st)
}else{
 dimnames(vo)=list(name=c(paste(name,fname,sep=", "),"non-transferable"),stage=st)
}
# if plot=T make webpages to go with vote plots, and if verbose=T display them
if(plot==T){
 wp=webpages(elecdata,va,vo,qa,itt,outdirec,sys="wig",map,electitle)
 if(verbose==T){grDevices::dev.off()}
 utils::browseURL(wp[[1]],browser="open")
}

elec=it[it>0]; x=elec
txt=matrix(txt,nrow=2)
pp=paste(" (",party[elec],")",sep=""); if(pp[[1]]==" ( )"|pp[[1]]==" ()"){pp=""}
elected=paste(fname[elec]," ",name[elec],pp,sep="",collapse=", ")
result=paste("Elected:",elected,sep="  ")
cat(result); cat("\n\n")

# save data and result to outdirec
result=list(elec=elected,itt=itt,txt=txt,votes=vo,va=va)
elecfile=paste(strsplit(elecname," ")[[1]],collapse="_")
save(result,file=paste0(outdirec,"/",elecfile,"_",sys,".rda"))
save(elecdata,file=paste0(outdirec,"/",elecfile,".rda"))
result
}
