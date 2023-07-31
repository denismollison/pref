# functions_output.R - last revised 28 june 2023
# output plotting functions voteplot, webpages, plot_jpeg; also plural (for grammatical output)
# 
voteplot=function(ns,vm,qpc,it,tx,name,party,colour,transf,elecname,cx=1,sys="meek"){
# 
qa=qpc  # qa is quota as %age
# consider getting rid of cx (regulates font scaling)
margincolour="burlywood";  panelcolour="burlywood1"
# convert vote variables to percentages
b=sum(vm); nc=dim(vm)[[1]]-1
vm=vm*100/b
vm0=vm;  diag(vm0)=0
ip={max(nchar(party))>0}
# allocate x coords of cands. - with extra space before "n-t"
x1=((1:nc)-1)*5; x1=c(x1,(nc+1)*5)
x2=x1+4
vmax=max(qa+5,apply(vm,2,sum)*1.05)
vmin=-5*max(1,0.1*max(vm))
# abbreviate long names
name[nchar(name)>12]=paste(substring(name[nchar(name)>12],1,11),"*",substring(name[nchar(name)>12],nchar(name[nchar(name)>12]),nchar(name[nchar(name)>12])),sep="")
cexn=2*cx; if(max(nchar(name))>10){cexn=cexn*10/max(nchar(name))}
# new bit, 22may2018, for transfers plot
tfsc=qa/100
tf0=vmax*1.5
tfmax=100*tfsc
ymax=tf0+tfmax
graphics::par(mar=c(0,11,1,5),bg=margincolour)
if(transf==1){yy=ymax}else{yy=vmax*1.4}
graphics::plot(0,0,axes=F,xlim=c(0,(nc+2)*5),ylim=c(vmin,yy),xlab="",ylab="",pch="")
if(transf==1){graphics::rect(-1,tf0,(nc+2)*5,yy,col=panelcolour)}
graphics::rect(-1,0,(nc+2)*5,vmax,col=panelcolour)
spread=1+0.5*max(vmax/(qa+5)-1,0)
graphics::mtext(elecname,side=2,line=9,at=-(7+2*ip)*spread,adj=0,cex=cx*1.3,las=1)
if(sys !="meek"){systext=paste("STV - sys =",sys)
graphics::mtext(systext,side=2,line=9,at=-(9+2*ip)*spread,adj=0,font=3,cex=cx*1.3,las=1)}
# axis and parallel lines for vote plot
# abline(h=qa*(0:floor(vmax/qa)),lwd=2)
graphics::abline(h=qa*(0:1),lwd=2)
graphics::abline(h=(1:floor(vmax)),lty=3)
q5=5*(0:floor(vmax/5))
graphics::abline(h=q5,lty=2)
graphics::abline(v=(nc*5+3),lty=1)
graphics::axis(side=2,at=q5,labels=q5,lty=0,las=1,cex.axis=1.8)
graphics::mtext(side=2,line=4,at=qa*0.5,text="votes %",cex=cx*2.2)
graphics::mtext(side=2,line=3,at=qa,text="quota",font=3,las=1,cex=cx*2)
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
  y1=diag(vm)
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


webpages=function(elecdata,va,vo,q0,itt,outdirec,sys="meek",map=FALSE,electitle=character()){
# outlines=readLines("outlines.txt")   # needed if not in RStudio
# to make a pair of election web pages (without/with transfers) -
# outlines, = non-varying lines of html, are available because in sysdata.rda
#   cat("in webpage\n")
space="&#160;&#160;"; space5="&#160;&#160;&#160;&#160;&#160;"      # needed for formatting
ed=elecdata
elecname=ed$e; ns=ed$s; nc=ed$c; mult=ed$m; fname=ed$f; name=ed$n; party=ed$p
elecfile=paste(strsplit(elecname," ")[[1]],collapse="_")
electitle=c(electitle,elecname)
if(map!=F){
 electitle=c(electitle,paste0('<a href="',ed$map,'">(map)</a>'))
 }
if(ed$nv>0){
 if(length(dim(va))==3){nstages=dim(va)[[3]]}else{nstages=1}
 it=itt[[nstages]]
 unc=""
}else{
 cat("in webpages line 134 - uncontested\n")
 unc="(uncontested)"
 it=1:ed$c
} 
tra=c("","t"); tra1=c("t",""); hide=c("Show","Hide")    
webpp=character()
for(j in 1:2){
 transf=j-1
 indext=paste("index",tra[[j]],".html",sep="")
 out_html=paste(outdirec,indext,sep="/")
 webpp=c(webpp,out_html)
# now make webpage `out_html'
sink(out_html)
for(i in 1:7){
cat(outlines[[i]],"\n",sep="")
}
cat("<h3>"); cat(electitle,sep=space5)
cat(outlines[[8]],"\n",sep="")
cat("<p><b><em>Elected",unc,": </em></b>",space,sep="")
elec=it[it>0]; x=elec
pp=paste(" (",party[x],")",sep=""); if(pp[[1]]==" ( )"|pp[[1]]==" ()"){pp=""}
cat(paste(fname[x]," ",name[x],pp,sep="",collapse=paste(", ","&#160;")))
if(ed$c<ed$s){for(i in 1:(ed$s-ed$c)){cat(", &#160;<em>vacancy</em>")}}
cat("\n"); cat(outlines[[9]],"\n",sep="")
if(ed$nv>0){ 
 cat(outlines[[10]],"\n",sep="")
 cat('<table bgcolor="red"><tr height=30><td width=5></td><td><a href="index',tra1[[j]],'.html">',hide[[j]],' transfers</a></td><td width=5></td></tr></table>\n',sep='')
 cat("Count stage",space,"\n",sep="")
 for(i in 1:nstages){
    cat('<button class="w3-button demo" onclick="currentDiv(',i,')">',i,'</button>\n',sep='')
 }
 cat('</div>\n<div>')
 for(i in 1:nstages){
     cat('<img class="mySlides" src="stage',tra[[j]],i,'.jpg" style="width:100%">\n',sep='')
 }
 for(i in 11:46){
  cat(outlines[[i]],"\n",sep="")
 }
 cat(outlines[47:48],"\n",sep="")
 options(width=140,max.print=5000)
 print(round(vo,2)); cat("<p>")
 tv0=sum(mult)
 if(nstages>1){
  qf=sum(va[1:nc,1:nc,nstages])/(ns+1)
 }else{qf=q0}
 cat("Total votes ",tv0,", &#160;",sep="")
 if(sys=="meek"){
  cat("initial quota = ",round(q0,2),", final quota = ",round(qf,2),"\n",sep="")
 }else{cat("quota = ",round(q0,2))}
 for(i in 49:60){
  cat(outlines[[i]],"\n",sep="")
 }
 cat("</div>"); cat("\n")
}else{cat("<br><br><br><br><br>\n")}

cat("<p>&#160;&#160;<b><em>Downloads:</em></b>",
paste0("<b>Result </b><a href=",elecfile,"_",sys,".rda>as an R list</a>"),"",
 "<b>Vote data file:</b>",
paste0("<a href=",elecfile,".rda>as an R list</a>"),
sep="&#160;&#160;"); cat("\n")

for(i in 61:62){
 cat(outlines[[i]],"\n",sep="")
}
sink()
}
webpp
} # end of function webpages


plot_jpeg=function(plotfile,stage)
 {dpi=200
  jpg = jpeg::readJPEG(plotfile, native=T) # read the file
  res = dim(jpg)[2:1] # get the resolution, [x, y]
  w=res[[1]]/dpi; h=res[[2]]/dpi
# if at stage 1 initialize plot of right size
#  (will be closed in stv when web pages are written)
  if(stage==1){grDevices::dev.new(width=w,height=h,noRStudioGD = TRUE)}
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
