# stv.plots - text of main statistics of election count
# 13 jan 2024, last revised 23 jan 2024

#' Makes webpage plots of result of an STV election
#'
#' @param elecdata An R list of results from stv or stv.wig
#' @param outdirec A directory for web page output
#' @param webdisplay If TRUE displays the main output web page
#' @param map A possible extra showing map of election location
#' @return Webpages with plots of election count and results
#' @export
#'
#' @examples c99result=stv(c99)
#' @examples stv.plots(c99result)
#' @examples nws17wig=stv.wig(nws17)
#' @examples stv.plots(nws17wig)

stv.plots=function(elecdata,outdirec=tempdir(),webdisplay=FALSE,map=FALSE){

if(!dir.exists(outdirec)){dir.create(outdirec)}

ed=elecdata

# vars from votedata
elecname=ed$e
ns=ed$s
nc=ed$c
nv=ed$nv
mult=ed$m
fname=ed$f
name=ed$n
n2=ed$n2
party=ed$p
col=ed$col

# additional vars from count
sys=ed$sys
elec=ed$el
itt=ed$itt # - may not need 
ctext=ed$ctext
csum=ed$csum
qtext=ed$qtext # - may not need
va=ed$va
if(sys=="meek"){keep=ed$keep}

# if we have all those ...
nstages=dim(csum)[[2]]-1
qs=apply(csum[1:nc,1:nstages],2,sum)/(ns+1); qs=100*qs/sum(mult)
cat("quota: ",round(qs,2));cat("\n")
wi=(nc+4.5); w=wi*120   # plot width in (approx) inches, and in pixels
trf=c("","t")
for(ist in 1:nstages){
    vm=va[,,ist]
    if(sys=="meek"){qpc=qs[[ist]]}else{qpc=qs[[1]]}
    cat("stage ",ist,", qpc: ",round(qpc,2)); cat("\n")
    it=itt[[ist]]
    dtext=ctext[,ist]                                     
for(i in 2:1){  # 2 plots, with/without separate transfers plot
    transf=i-1
    plotfile=paste(outdirec,paste0("stage",trf[[i]],ist,".jpg"),sep="/")
    h=600+200*transf
    grDevices::jpeg(plotfile,width=w,height=h)
    voteplot(ns,vm,qpc,it,dtext,n2,party,col,transf,elecname,sys)
    grDevices::dev.off()
}}
    
wp=webpages(elecdata,outdirec,map)
 if(webdisplay==TRUE){utils::browseURL(wp[[1]],browser="open")}

}
