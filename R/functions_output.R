# functions_output.R - last revised 29 jan 2024
# output plotting functions voteplot, webpages, plot_jpeg; also plural (for grammatical output)
# voteplot - makes plots of a stage of the count with and without transfer plot
#' voteplot
#'
#' makes a plot of one stage of an election count.
#'
#' @param ns  Number of seats
#' @param vm  Vote transfers matrix
#' @param qpc  Quota as per cent
#' @param it  Candidaates eletedc or excluded so far
#' @param tx  Text explanation of stage
#' @param name  Candidates' names
#' @param party  Candidates' parties
#' @param colour  Candidates' party colours
#' @param transf  Whether to include plot of transfers
#' @param elecname  Election name
#' @param sys  STV system
#'
#' @return  Makes a plot of current stage
#' @noRd
voteplot=function(ns,vm,qpc,it,tx,name,party,colour,transf,elecname,sys="meek"){

cx=1 # consider getting rid of cx (regulates font scaling)
margincolour="burlywood";  panelcolour="burlywood1"
# convert vote variables to percentages
b=sum(vm); nc=dim(vm)[[1]]  # changed vm
vm=vm*100/b
vm0=vm;  diag(vm0[,1:nc])=0
ip={max(nchar(party))>0}
# allocate x coords of cands. - with extra space before "n-t"
x1=((1:nc)-1)*5; x1=c(x1,(nc+1)*5)
x2=x1+4
vmax=max(qpc+5,apply(vm,2,sum)*1.05)
vmin=-5*max(1,0.1*max(vm))
# abbreviate long names
name[nchar(name)>12]=paste(substring(name[nchar(name)>12],1,11),"*",substring(name[nchar(name)>12],nchar(name[nchar(name)>12]),nchar(name[nchar(name)>12])),sep="")
cexn=2*cx; if(max(nchar(name))>10){cexn=cexn*10/max(nchar(name))}
# new bit, 22may2018, for transfers plot
tfsc=qpc/100
tf0=vmax*1.5
tfmax=100*tfsc
ymax=tf0+tfmax
oldpar=graphics::par(no.readonly = TRUE) # to record existing setting ..
on.exit(graphics::par(oldpar)) # .. and restore when exit function
graphics::par(mar=c(0,11,1,5),bg=margincolour)
if(transf==1){yy=ymax}else{yy=vmax*1.4}
graphics::plot(0,0,axes=F,xlim=c(0,(nc+2)*5),ylim=c(vmin,yy),xlab="",ylab="",pch="")
if(transf==1){graphics::rect(-1,tf0,(nc+2)*5,yy,col=panelcolour)}
graphics::rect(-1,0,(nc+2)*5,vmax,col=panelcolour)
spread=1+0.5*max(vmax/(qpc+5)-1,0)
graphics::mtext(elecname,side=2,line=9,at=-(7+2*ip)*spread,adj=0,cex=cx*1.3,las=1)
if(sys !="meek"){systext=paste("STV - sys =",sys)
graphics::mtext(systext,side=2,line=9,at=-(9+2*ip)*spread,adj=0,font=3,cex=cx*1.3,las=1)}
# axis and parallel lines for vote plot
# abline(h=qpc*(0:floor(vmax/qpc)),lwd=2)
graphics::abline(h=qpc*(0:1),lwd=2)
graphics::abline(h=(1:floor(vmax)),lty=3)
q5=5*(0:floor(vmax/5))
graphics::abline(h=q5,lty=2)
graphics::abline(v=(nc*5+3),lty=1)
graphics::axis(side=2,at=q5,labels=q5,lty=0,las=1,cex.axis=1.8)
graphics::mtext(side=2,line=4,at=qpc*0.5,text="votes %",cex=cx*2.2)
graphics::mtext(side=2,line=3,at=qpc,text="quota",font=3,las=1,cex=cx*2)
#   name, party, whether elected by this stage
elec=it[it>0]
y1=-vmax*(0.1+0.07*(0:2))
graphics::text(x1+2,y1[[1]],c(name,"non-"),cex=cx*1.3)
graphics::text(x1+2,y1[[2]],c(party,"transferable"),cex=cx*1.3)
graphics::text(x1[elec]+2,y1[[2+ip]],"(elec)",cex=cx*1.3)
# kept votes
for(i in 1:nc){
graphics::rect(x1[i],0,x2[i],vm[i,i],col=colour[i])
}
# votes received from transfers (in order of elec/excl)
if(length(it)>0){
  y1=c(diag(vm[,1:nc]),0)
  for(i in 1:length(it)){
    io=abs(it[[i]])
    y2=y1+vm0[io,]
    graphics::rect(x1,y1,x2,y2,col=colour[[io]])
    y1=y2
  }
}
# explanation of this stage of count
dec1=tx[[1]]; dec2=tx[[2]]
cex1=1.8*min(1,(8+13*nc)/nchar(dec1))
cex2=1.8*min(1,(7+11*nc)/nchar(dec2))
graphics::text((nc+0.5)*2.5,vmax*1.3,dec1,cex=cex1)
graphics::text((nc+0.5)*2.5,vmax*1.17,dec2,cex=cex2)
# explanatory box for votes plot
xt1=(nc*5)+1; xt2=(nc+2)*5-2
    y=vmax*(1.1-0.035*c(0,2,5,7,9,11))
vnt=sum(vm[,(nc+1)])*1.05
if(y[[6]]<vnt){y=y+(vnt-y[[6]])}
graphics::rect(xt1,y[[6]],xt2,y[[1]],col=margincolour)
graphics::text(xt1,y[[2]],pos=4,"votes:",font=2,cex=cx*1.8)
graphics::text(xt1,y[[3]],pos=4,"colour shows",cex=cx*1.5)
graphics::text(xt1,y[[4]],pos=4,"where votes",cex=cx*1.5)
graphics::text(xt1,y[[5]],pos=4,"have come from",cex=cx*1.5)
# now add transfers plot - if transf==1
# plotted in order in which originators started to transfer
if(transf==1){
colour2=c(colour,"grey")
vf=apply(vm,1,sum)
xv1=x1[1:nc]+2-2*(ns+1)*vf[1:nc]/sum(vm)
xv2=x1[1:nc]+2+2*(ns+1)*vf[1:nc]/sum(vm)
v1=apply(vm0,1,cumsum)
y2=tf0+rep(0,nc+1)*tfsc
for(i in 1:(nc+1)){
y1=y2
y2=tf0+100*v1[i,]*tfsc/vf
graphics::rect(xv1,y1,xv2,y2,border=NA,col=colour2[[i]])
}
graphics::abline(h=tf0+tfsc*10*(0:10),lty=3)
tfpc=20*(0:5); ytfpc=tf0+tfsc*tfpc
graphics::axis(side=2,at=ytfpc,labels=tfpc,lty=0,las=1,cex.axis=1.8)
graphics::mtext(side=2,line=4.3,at=tf0+tfsc*45,text="transfers %",cex=2.2)
# explanatory box for transfers plot
y=ymax-vmax*0.035*c(2,4,7,9,11,13,16,18,20)
if(y[[9]]<=tf0){y=y+vmax*0.07}
graphics::rect(xt1,y[[9]],xt2,y[[1]],col=margincolour)
graphics::text(xt1,y[[2]],pos=4,"transfers:",font=2,cex=cx*1.5)
graphics::text(xt1,y[[3]],pos=4,"colour shows",cex=1.5)
graphics::text(xt1,y[[4]],pos=4,"where transfers",cex=1.5)
graphics::text(xt1,y[[5]],pos=4,"have gone (grey=",cex=1.5)
graphics::text(xt1,y[[6]],pos=4,"non-transferable);",cex=1.5)
graphics::text(xt1,y[[7]],pos=4,"area shows",cex=1.5)
graphics::text(xt1,y[[8]],pos=4,"amount of votes",cex=1.5)
}
} # end of function voteplot


webpages=function(elecdata,outdirec,map=FALSE){
# to make a pair of election web pages (without/with transfers) -
# outlines, = non-varying lines of html, are available because in sysdata.rda
space="&#160;&#160;"; space5="&#160;&#160;&#160;&#160;&#160;"      # needed for formatting

ed=elecdata
elecname=ed$e; ns=ed$s; nc=ed$c;nv=ed$nv; mult=ed$m
fname=ed$f; name=ed$n; party=ed$p
sys=ed$sys; itt=ed$itt; csum=ed$count
qtext=ed$quotatext; va=ed$va; if(sys=="meek"){keep=ed$keep}

elecfile=paste(strsplit(elecname," ")[[1]],collapse="_")
if(map!=FALSE){
 electitle=c(elecname,paste0('<a href="',ed$map,'">(map)</a>'))
 }else{electitle=elecname}
if(nv>0){
 if(length(dim(va))==3){nstages=dim(va)[[3]]}else{nstages=1}
 it=itt[[nstages]]
 unc=""
}else{
 unc="(uncontested)"
 it=1:ed$c
}
tra=c("","t"); tra1=c("t",""); hide=c("Show","Hide")    
    
for(j in 1:2){
transf=j-1
html=outlines[1:7]
html=c(html,paste0("<h3>",paste(electitle,sep=space5),"</h3>"))
html=c(html,outlines[[8]])
elec=it[it>0]; x=elec
pp=paste(" (",party[x],")",sep=""); if(pp[[1]]==" ( )"|pp[[1]]==" ()"){pp=""}
text1=paste0("<p><b><em>Elected",unc,": </em></b>",space)
text2=paste(fname[x]," ",name[x],pp,sep="",collapse=paste(", ","&#160;"))
nvac=max(ns-nc,0); text3=rep(", &#160;<em>vacancy</em>",nvac)
html=c(html,paste0(text1,text2,text3))
html=c(html,outlines[[9]])
if(nv>0){ 
html=c(html,outlines[[10]])
html=c(html,paste0('<table bgcolor="red"><tr height=30><td width=5></td><td><a href="index',tra1[[j]],'.html">',hide[[j]],' transfers</a></td><td width=5></td></tr></table>'))
html=c(html,paste0("Count stage",space))
for(i in 1:nstages){
 html=c(html,paste0('<button class="w3-button demo" onclick="currentDiv(',i,')">',i,'</button>\n'))
}
html=c(html,('</div>\n<div>'))
for(i in 1:nstages){
 html=c(html,paste0('<img class="mySlides" src="stage',tra[[j]],i,'.jpg" style="width:100%">\n'))
}
html=c(html,outlines[11:48])

width0=getOption("width")
options(width=150)
on.exit(options(width=width0))
csvec=utils::capture.output(
  print(round(csum,2),collapse="\n")
)
html=c(html,csvec)
options(width=width0)

html=c(html,"<p>",qtext)
html=c(html,outlines[49:60],"</div>")
}else{html=c(html,"<br><br><br><br><br>")}

text1="<p>&#160;&#160;<b><em>Download:</em></b>"
text2=paste0("<a href=",elecfile,"_",sys,".rda><b>Vote and count data </b>as an R list</a>")
html=c(html,paste(text1,text2,sep="&#160;&#160;"))

html=c(html,outlines[61:62])

indext=paste("index",tra[[j]],".html",sep="")
out_html=paste(outdirec,indext,sep="/")
writeLines(html,out_html)
} # end of loop making index.html and indext.html
} # end of function webpages


plot_jpeg=function(plotfile,stage){
# to display a plot showing one stage of the count
dpi=200
jpg = jpeg::readJPEG(plotfile, native=TRUE) # read the file
res = dim(jpg)[2:1] # get the resolution, [x, y]
w=res[[1]]/dpi; h=res[[2]]/dpi
# if at stage 1 initialize plot of right size
#  (will be closed in stv when web pages are written)
if(stage==1){grDevices::dev.new(width=w,height=h,noRStudioGD = TRUE)}
oldpar=graphics::par(no.readonly = TRUE) # record existing settings of par ..
on.exit(graphics::par(oldpar)) # .. and restore when exit function
graphics::par(mar=rep(0,4))
graphics::plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
graphics::rasterImage(jpg,1,1,res[1],res[2])
}


plural=function(names){
# function plural - for grammatical detail of output
n=length(names)
outnames=paste(names[[n]])
if(n>1){outnames=paste(names[[n-1]],"and",outnames)}
if(n>2){
  for(i in (n-2):1){outnames=paste(names[[i]],", ",outnames,sep="")}}
  if(n==1){is="is"; has="has"; es=""}else{is="are"; has="have";es="es"}
  list(out=outnames,is=is,has=has,es=es)
}


# stv.report - text report of main statistics of STV election count -
# 16 jan 2024 as stv.result; rewritten to avoid written output 29 jan 2024

stv.report=function(elecdata){

ed=elecdata
elecname=ed$e
ns=ed$s
nc=ed$c
mult=ed$m
name=ed$n
sys=ed$sys
ct=ed$narrative; nst=dim(ct)[[2]]
csum=ed$count
qtext=ed$quotatext
if(sys=="meek"){keep=ed$keep}

report=""
report=c(report,paste("Election:",elecname))
report=c(report,paste("System:",sys,"STV"))
report=c(report,paste("To fill",ns,"seats; ",nc,"candidates:"))
report=c(report,paste(name,collapse=", "))
totalvotes=sum(mult)
if(sys=="meek"){q0=totalvotes/(ns+1); q0text="votes;  initial quota"}else{
  q0=1+floor(totalvotes/(ns+1)); q0text="votes;  quota"}
report=c(report,paste(totalvotes,q0text,round(q0,2)),"")

# count text:
for(i in 1:nst){
report=c(report,paste(ct[1,i]),paste("  ",ct[2,i]),"")
}
report=c(report,paste("Those elected, in order of election:"),paste(ed$el),"")

if(sys=="meek"){
ic=1:nc; nst=dim(keep)[[2]]
ir=ic[keep[ic,nst]==100]
if(length(ir)>0){rname=dimnames(csum)[[1]][ir]}
report=c(report,paste("Runner-up:",paste(rname,collapse=", ")))
report=c(report,"")
}

if(sys=="meek"){
report=c(report,paste(("Votes at each stage and final keep values:")))
}else{
report=c(report,paste("Votes at each stage:"))
}

csvec=utils::capture.output(
  print(round(csum,2),collapse="\n")
)

report=c(report,csvec,"",paste(qtext))

report
}
