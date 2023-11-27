# stv() - core of STV package - last revised 25 nov 2023

#' STV election count - uses Meek STV, allows equal preferences
#'
#' @param elecdata File with vote data
#' @param outdirec Needs to be set for permanent record of results
#' @param verbose If =T reports and pauses at each stage of the count
#' (press return to continue to next stage)
#' @param plot If =T (default) produces plots of count and webpages in outdirec
#' @param webdisplay If =T displays plots and statistics as web pages
#' @param electitle For web page heading links if appropriate
#' @param map Link to a map or other URL associated with election
#' @param timing Whether to report computing time at each stage
#'
#' @return A list containing votes and keep vals at each stage, + optional web pages; for details see manual pref_pkg_manual.pdf (section 3)
#' @export
#'
#' @examples hc12m=stv(hc12,plot=FALSE)
#' @examples j02m=stv(j02,plot=FALSE)
#' @examples cnc17m=stv(cnc17,plot=FALSE)
#' @examples y12m=stv(y12,plot=FALSE)
#'
stv=function(elecdata,outdirec="out_stv",electitle=character(),map=F,verbose=F,plot=T,webdisplay=F,timing=F){
sys="meek"
tim0=proc.time()    # to track computing time taken (use timing=T to print for each stage)
# read and unpack elecdata - only essential component is vote matrix ed$v
ed=elecdata; vote=ed$v
ned=names(ed)
if("s" %in% ned){ns=ed$s}else{ns=as.numeric(readline("number of seats? "))}
nv0=dim(vote)[[1]]; nc0=dim(vote)[[2]]
if("e" %in% ned){elecname=ed$e}else{elecname="election"}
if("c" %in% ned){nc=ed$c}else{nc=nc0}
if("nv" %in% ned){nv=ed$nv}else{nv=nv0}
if("m" %in% ned){mult=ed$m}else{mult=rep(1,nv)}
na2=dimnames(vote)[[2]]
if(is.null(na2)){na2=LETTERS[1:nc]}
if("n" %in% ned){name=ed$n}else{if("n2" %in% ned){name=ed$n2}else{name=na2}}
if("n2" %in% ned){name2=ed$n2}else{name2=name}
if("f" %in% ned){fname=ed$f}else{fname=rep("",nc)}
if("p" %in% ned){party=ed$p}else{party=rep("",nc)}; np={party[[1]]==""}
if("col" %in% ned){colour=ed$col}else{colour=grDevices::rainbow(nc)}
ed=list(e=elecname,s=ns,c=nc,nv=nv,m=mult,v=vote,f=fname,n=name,n2=name2,p=party,col=colour)

totalvotes=sum(mult)
q0=totalvotes/(ns+1)	# initial quota

# print election name and basic statistics
cat(elecname)
cat(paste("  (",ns," seats, ",nc," candidates, ",totalvotes," votes)",sep="")); cat("\n")
if(verbose==T){cat("initial quota ",round(q0,2)); cat("\n\n")}

# initial quota, keep values (=1), and housekeeping variables
qa=q0
k=rep(1,(nc+1))		# initial keep values (the +1 is for non-transferable)
ks=numeric() 		# record keep value at each stage
ems=c(min(0.01,qa*0.001),0.0000001)	# initial error margin will be decreased ..
hp=1 		        # .. if close call (when hp is changed from 1 to 2)
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
if(!dir.exists(outdirec)){dir.create(outdirec)}

# main cycle - to elect or exclude next candidate(s)
while(ne<ns){
 em=ems[[hp]]
# recalculate keep values and transfer surpluses
 tr=transfer(k,iter,vote,mult,ns,ie,em,surplus,sel)
  k=tr$k; vm=tr$vm; vc=tr$vc; qa=tr$qa; inn=tr$inn
  iter=tr$iter; surplus=tr$sur
# make next decision to elect or exclude
 hp0=hp
 dn=decision(nc,vc,qa,ie,k,stage,fin,vo,st,surplus,hp)
 hp=dn$hp
 if(hp!=hp0){if(verbose==T){cat("close call - need high precision"); cat("\n\n")}
 }else{
  k=dn$k; ie=dn$ie; elec=dn$elec; xcl=dn$xcl; it=c(it,elec,xcl*ie[xcl])
  stage=dn$stage; vo=dn$vo; st=dn$st
  ne=length(ie[ie==1])
  ks=cbind(ks,k)
  x=decision_text(stage,ne,ns,elec,xcl,name2,dnext)
  dnext=x$d; dtext=x$t; txt=c(txt,dtext)
  if(stage==1){
   va=vm; itt=list(it)
  }else{
   va=array(c(va,vm),dim=c((nc+1),(nc+1),stage))
   itt=append(itt,list(it))
  }
  qpc=100*qa/totalvotes
  tim=proc.time()-tim0;  pt=tim[[1]]
# if plot=T : make permanent plots of stage
  if(plot==T){
   wi=(nc+4.5); w=wi*120   # plot width in (approx) inches, and in pixels
   for(i in 2:1){  # 2 plots, with/without separate transfers plot
       transf=i-1
       plotfile=paste(outdirec,paste("stage",trf[[i]],stage,".jpg",sep=""),sep="/")
    h=600+200*transf
    grDevices::jpeg(plotfile,width=w,height=h)
    voteplot(ns,vm,qpc,it,dtext,name2,party,colour,transf,elecname,sys="meek")
    grDevices::dev.off()
  }}
if(timing==T){cat(stage,"    process time ",pt," secs    "); cat("\n")}

# if verbose=T : print decision, require interaction (CR) at each stage
  if(verbose==T){
  if(stage==1){cat(dtext,sep="")}else{cat(dtext,sep=", ")}; cat("\n\n")
# .. and plot current state of votes if plot=T
  if(plot==T){plot_jpeg(plotfile,stage)}
   readline("next? ")
  }
 }}
fin=1; nstages=stage;  qf=qa   # final values of count proper

# extra stage to calculate final keep values
if(length(ie[ie>=0])>ns){
 tr=transfer(k,iter,vote,mult,ns,ie,em,surplus,sel)
 k=tr$k; vmp=tr$vmp; vc=tr$vc; qa=tr$qa; inn=tr$inn; iter=tr$iter; surplus=tr$sur
 while(length(k[k>0])>(ns+2)){
  dn=decision(nc,vc,qa,ie,k,stage,fin,vo,st,surplus,hp)
  k=dn$k; ie=dn$ie; elec=dn$elec; xcl=dn$xcl; it=c(it,elec,xcl*ie[xcl])
  surplus=dn$surplus; stage=dn$stage; vo=dn$vo; st=dn$st
  tr=transfer(k,iter,vote,mult,ns,ie,em,surplus,sel)
  k=tr$k; vmp=tr$vmp; vc=tr$vc; qa=tr$qa; inn=tr$inn; iter=tr$iter; surplus=tr$sur
 }
 tim=proc.time()-tim0;  pt=tim[[1]]
}

# print final result
elec=it[it>0]; x=elec
if(np==F){pp=paste(" (",party,")",sep="")}else{pp=party}
elected=paste(fname[x]," ",name[x],pp[x],sep="",collapse=", ")
eleclist=paste("Elected:",elected,sep="  ")
cat(eleclist); cat("\n\n")
# Runner-up
ic=1:nc; ru=ic[k[ic]==1]
if(verbose==T){cat(paste("Runner-up: ",fname[ru]," ",name[ru],pp[ru],sep="",collapse=", ")); cat("\n")}

# finalise matrices of keep values and votes at each stage (ks, vo)
ks=cbind(ks,k)
dimnames(ks)=list(name=c(paste(name,fname,sep=", "),"non-transferable"),stage=1:dim(ks)[[2]])
vo=cbind(vo,100*k); st=c(st,"  final keep values (%)")
txt=matrix(txt,nrow=2)
if(length(party[party!=""])>0){dimnames(vo)=list(name=c(paste(name,", ",fname," (",party,") ",sep=""),"non-transferable"),stage=st)
}else{
 dimnames(vo)=list(name=c(paste(name,fname,sep=", "),"non-transferable"),stage=st)
}
if(verbose==T){cat("\nVotes at each stage and final keep values:\n")
print(round(vo,2))}

# save result details and elecdata in R data files
result=list(elec=elected,itt=itt,txt=txt,votes=vo,va=va,keep=ks[1:nc,]*100)
elecfile=paste(strsplit(elecname," ")[[1]],collapse="_")
save(result,file=paste0(outdirec,"/",elecfile,"_",sys,".rda"))
save(elecdata,file=paste0(outdirec,"/",elecfile,".rda"))

# if plot=T make webpages to go with vote plots,
#   and if verbose=T display them
if(plot==T){
 wp=webpages(elecdata,va,vo,q0,itt,outdirec,sys,map,electitle)
 if(verbose==T){grDevices::dev.off()}
 if(webdisplay==T){utils::browseURL(wp[[1]],browser="open")}
 }
result
}
