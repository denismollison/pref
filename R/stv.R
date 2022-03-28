# stv() - core of STV package, rewritten as a function from original stv20.R

#' STV election count
#'
#' @param elecdata File with vote data
#' @param verbose Whether to report on each stage of the count
#' @param plot Whether to produce web pages with plots of countcheck()
#' @param outdirec Directory for webpages (if produced)
#'
#' @return A list containing votes and keep vals at each stage, + optional web pages
#' @export
#'
#' @examples j02c=stv(j02)
#' @examples hc12c=stv(hc12)
#' @examples p17c=stv(p17)
#' @examples y12c=stv(y12)
#'
stv=function(elecdata,outdirec=tempdir(),verbose=T,plot=T){
    tim0=proc.time()    # to track computing time taken
    x=outlines
# read and unpack elecdata
ed=elecdata; elecname=ed$e
ns=ed$s; nc=ed$c; vote=ed$v; mult=ed$m; nv=ed$nv
name=ed$n; fname=ed$f; name2=ed$n2
party=ed$p; colour=ed$col; np={party[[1]]==""}
# initial quota, keep values (=1), and housekeeping variables
    q0=sum(mult)/(ns+1)	# initial quota
if(verbose==T){cat(elecname); cat(paste("  (",ns," seats, ",nc," candidates)",sep="")); cat("\n")
    cat("total votes ",sum(mult),",  initial quota ",round(q0,2)); cat("\n\n")}
    qa=q0
k=rep(1,(nc+1))		# initial keep values (the +1 is for non-transferable)
ks=numeric() 		# record keep value at each stage
em=min(0.01,qa*0.001)	# initial error margin (will be decreased if necessary)
surplus=1		# to ensure calculation gets going
iter=0			# keeps track of number of iterations in count
it=numeric()		# it=elec(+) and excl(-) in order of being decided
ie=rep(0,nc); ne=0	# indicator (ie=1 indicates elected, =-1 excluded)
st=character()          # ? for stages
vo=numeric()            # to record votes at each stage; note also vm, va
stage=0; fin=0
elec=numeric()
sel=select(ns)          # ways of selecting subsets - needed in share
dnext=""                # text carried over from one stage to next
txt=character()         # text describing decisions at each stage
itt=list()              # cand nos in order of elec/excl for each stage
trf=c("","t")

    if(plot==T & !dir.exists(outdirec)){dir.create(outdirec)}
# system(paste("mkdir",outdirec))}

# main cycle - to elect or exclude next candidate(s)
while(ne<ns){
# recalculate keep values and transfer surpluses
    tr=transfer(k,iter,surplus,vote,mult,ns,ie,em,sel)
    k=tr$k; vm=tr$vm; vc=tr$vc; qa=tr$qa; inn=tr$inn
    iter=tr$iter; surplus=tr$sur
# make next decision to elect or exclude
    dn=decision(nc,vc,qa,ie,surplus,k,em,stage,fin,vo,st)
    k=dn$k; ie=dn$ie; elec=dn$elec; xcl=dn$xcl; it=c(it,elec,xcl*ie[xcl])
    surplus=dn$surplus; stage=dn$stage; vo=dn$vo; st=dn$st
    ne=length(ie[ie==1])
    ks=cbind(ks,k)
    x=decision_text(stage,ne,ns,elec,xcl,name2,dnext)
    dnext=x$d; txt=c(txt,x$t)
    if(stage==1){
      va=vm; itt=list(it)
    }else{
      va=array(c(va,vm),dim=c((nc+1),(nc+1),stage))
      itt=append(itt,list(it))
    }
    tim=proc.time()-tim0;  pt=tim[[1]]
# make permanent plots of stage (if plot=T)
if(plot==T){
  wi=(nc+4.5); w=wi*120   # plot width in (approx) inches, and in pixels
  for(i in 2:1){
  transf=i-1
  plotfile=paste(outdirec,paste("stage",trf[[i]],stage,".jpg",sep=""),sep="/")
  h=600+200*transf
  grDevices::jpeg(plotfile,width=w,height=h)
    voteplot(ns,vm,qa,it,x$t,name2,party,colour,transf,elecname)
  grDevices::dev.off()
}}
# print decision (if verbose=T)
    if(verbose==T){
    cat("iteration ",iter,"    process time ",pt," secs    "); cat("\n")
    cat(x$t,sep=", "); cat("\n\n")
# .. and plot current state of votes if plot=T
    if(plot==T){plot_jpeg(plotfile,stage)}
   readline("next? ")
  }
}
fin=1; nstages=stage;  qf=qa   # final values of count proper

# extra stage to calculate final keep values
tr=transfer(k,iter,surplus,vote,mult,ns,ie,em,sel)
  k=tr$k; vmp=tr$vmp; vc=tr$vc; qa=tr$qa; inn=tr$inn; iter=tr$iter; surplus=tr$sur
while(length(k[k>0])>(ns+2)){
  dn=decision(nc,vc,qa,ie,surplus,k,em,stage,fin,vo,st)
    k=dn$k; ie=dn$ie; elec=dn$elec; xcl=dn$xcl; it=c(it,elec,xcl*ie[xcl])
    surplus=dn$surplus; stage=dn$stage; vo=dn$vo; st=dn$st
  tr=transfer(k,iter,surplus,vote,mult,ns,ie,em,sel)
    k=tr$k; vmp=tr$vmp; vc=tr$vc; qa=tr$qa; inn=tr$inn; iter=tr$iter; surplus=tr$sur
  }
tim=proc.time()-tim0;  pt=tim[[1]]
# print final keep values if verbose=T
if(verbose==T){cat("iteration ",iter,"    process time ",pt," secs"); cat("\n")
cat("final keep values (%):"); cat("\n"); print(round(100*k[1:nc],2)); cat("\n")}
# .. and final result
elec=it[it>0]; x=elec
if(np==F){pp=paste(" (",party,")",sep="")}else{pp=party}
#   if(pp[[1]]==" ( )"|pp[[1]]==" ()"){pp=""}
elected=paste(fname[x]," ",name[x],pp[x],sep="",collapse=", ")
result=paste("Elected:",elected,sep="  ")
cat(result); cat("\n\n")
# Runner-up ? - might add this
ic=1:nc; ru=ic[k[ic]==1]
cat(paste("Runner-up: ",fname[ru]," ",name[ru],pp[ru],sep="",collapse=", ")); cat("\n")
# finalise matrices of keep values and votes at each stage (ks, vo)
ks=cbind(ks,k)
dimnames(ks)=list(name=c(paste(name,fname,sep=", "),"non-transferable"),stage=1:dim(ks)[[2]])
vo=cbind(vo,100*k); st=c(st,"  final keep values (%)")
if(length(party[party!=""])>0){dimnames(vo)=list(name=c(paste(name,", ",fname," (",party,") ",sep=""),"non-transferable"),stage=st)}else{
dimnames(vo)=list(name=c(paste(name,fname,sep=", "),"non-transferable"),stage=st)
}
# if plot=T make webpages to go with vote plots, and if verbose=T display them
if(plot==T){wp=webpages(elecdata,va,vo,qa,itt,outdirec,sys="meek")
if(verbose==T){grDevices::dev.off()
  utils::browseURL(wp[[1]],browser="open")}}
txt=matrix(txt,nrow=2)   # store decision text as matrix
list(elec=result,itt=itt,votes=vo,va=va,keep=ks[1:nc,]*100)
}
